Require Import pasta.Pasta.

Notation IDimage_band_box_z := 1%positive.
Notation IDimage_band_box__tmp := 2%positive.
Notation IDimage_band_box__tmp1 := 3%positive.
Notation IDimage_band_box_by0 := 4%positive.
Notation IDimage_band_box_by1 := 5%positive.
Notation IDimage_band_box_i := 6%positive.
Notation IDimage_band_box_px := 7%positive.
Notation IDimage_band_box_py := 8%positive.
Notation IDimage_band_box_qx := 9%positive.
Notation IDimage_band_box_qy := 10%positive.
Notation IDimage_band_box_dev := 11%positive.
Notation IDimage_band_box_h := 12%positive.
Notation IDimage_band_box_pbox := 13%positive.
Notation IDimage_band_box_pie := 14%positive.
Notation IDimage_band_box_y := 15%positive.
Definition image_band_box : graph := {|
  g_start := 1%positive;
  g_end := 142%positive;
  g_edges := (1%positive,(AAssign IDimage_band_box_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDimage_band_box__tmp1
             (Some (EVar IDimage_band_box_y))),3%positive)::
             (3%positive,(AAssign IDimage_band_box__tmp
             (Some (EVar IDimage_band_box_h))),4%positive)::
             (4%positive,(AAssign IDimage_band_box_by0 None),5%positive)::
             (5%positive,(AAssign IDimage_band_box_by1 None),6%positive)::
             (6%positive,(AAssign IDimage_band_box_px None),7%positive)::
             (7%positive,(AAssign IDimage_band_box_py None),8%positive)::
             (8%positive,(AAssign IDimage_band_box_qx None),9%positive)::
             (9%positive,(AAssign IDimage_band_box_qy None),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,14%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,16%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,19%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,21%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,23%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,26%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,119%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (26%positive,ANone,29%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,118%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDimage_band_box_i (Some (ENum (0)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDimage_band_box_i)
             s) < (eval (ENum (4)) s))%Z)),35%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDimage_band_box_i)
             s) >= (eval (ENum (4)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,139%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,38%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,49%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,41%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,49%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,44%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,49%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,47%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,49%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,51%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,62%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,54%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,62%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,57%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,AWeaken,62%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,ANone,60%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,62%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,64%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,AWeaken,88%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,68%positive)::
             (67%positive,ANone,75%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,70%positive)::
             (69%positive,ANone,75%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,72%positive)::
             (71%positive,ANone,75%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,74%positive)::
             (73%positive,ANone,75%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,ANone,79%positive)::
             (78%positive,ANone,86%positive)::
             (79%positive,AWeaken,80%positive)::
             (80%positive,ANone,81%positive)::
             (80%positive,ANone,86%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,ANone,83%positive)::
             (82%positive,ANone,86%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,ANone,85%positive)::
             (84%positive,ANone,86%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,ANone,87%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,ANone,89%positive)::
             (88%positive,ANone,112%positive)::
             (89%positive,ANone,90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,AWeaken,92%positive)::
             (92%positive,ANone,93%positive)::
             (92%positive,ANone,100%positive)::
             (93%positive,AWeaken,94%positive)::
             (94%positive,ANone,95%positive)::
             (94%positive,ANone,100%positive)::
             (95%positive,AWeaken,96%positive)::
             (96%positive,ANone,97%positive)::
             (96%positive,ANone,100%positive)::
             (97%positive,AWeaken,98%positive)::
             (98%positive,ANone,99%positive)::
             (98%positive,ANone,100%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,ANone,102%positive)::
             (102%positive,AWeaken,103%positive)::
             (103%positive,ANone,104%positive)::
             (103%positive,ANone,111%positive)::
             (104%positive,AWeaken,105%positive)::
             (105%positive,ANone,106%positive)::
             (105%positive,ANone,111%positive)::
             (106%positive,AWeaken,107%positive)::
             (107%positive,ANone,108%positive)::
             (107%positive,ANone,111%positive)::
             (108%positive,AWeaken,109%positive)::
             (109%positive,ANone,110%positive)::
             (109%positive,ANone,111%positive)::
             (110%positive,ANone,111%positive)::
             (111%positive,ANone,112%positive)::
             (112%positive,ANone,113%positive)::
             (113%positive,(AAssign IDimage_band_box_i
             (Some (EAdd (EVar IDimage_band_box_i) (ENum (1))))),
             114%positive)::(114%positive,ANone,115%positive)::
             (115%positive,ANone,116%positive)::
             (116%positive,(AAssign IDimage_band_box_z (Some (EAdd (ENum (1))
             (EVar IDimage_band_box_z)))),117%positive)::
             (117%positive,AWeaken,32%positive)::
             (118%positive,AWeaken,120%positive)::
             (119%positive,AWeaken,120%positive)::
             (120%positive,ANone,123%positive)::
             (120%positive,ANone,121%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,AWeaken,125%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,AWeaken,125%positive)::
             (125%positive,ANone,128%positive)::
             (125%positive,ANone,126%positive)::
             (126%positive,ANone,127%positive)::
             (127%positive,AWeaken,130%positive)::
             (128%positive,ANone,129%positive)::
             (129%positive,AWeaken,130%positive)::
             (130%positive,ANone,133%positive)::
             (130%positive,ANone,131%positive)::
             (131%positive,ANone,132%positive)::
             (132%positive,AWeaken,135%positive)::
             (133%positive,ANone,134%positive)::
             (134%positive,AWeaken,135%positive)::
             (135%positive,ANone,137%positive)::
             (135%positive,ANone,136%positive)::
             (136%positive,ANone,138%positive)::
             (137%positive,ANone,138%positive)::
             (138%positive,ANone,139%positive)::
             (139%positive,ANone,140%positive)::
             (140%positive,ANone,141%positive)::
             (141%positive,AWeaken,142%positive)::nil
|}.

Definition image_band_box_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 3%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 4%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 5%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 6%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 7%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 8%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 9%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 10%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 11%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 12%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 13%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 14%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 15%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 16%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 17%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 18%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 19%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 20%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 21%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 22%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 23%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 24%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 25%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 26%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 27%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 28%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 29%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 30%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 31%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ 1 * (s IDimage_band_box_i) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 32%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0 /\ 1 * (s IDimage_band_box_i) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDimage_band_box_i) + -4 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) + 4 <= 0)%Z
    | 34%positive => (-1 * (s IDimage_band_box_i) + 4 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -4 <= 0)%Z
    | 35%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 36%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 37%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 38%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 39%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 40%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 41%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 42%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 43%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 44%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 45%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 46%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 47%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 48%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 49%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 50%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 51%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 52%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 53%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 54%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 55%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 56%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 57%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 58%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 59%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 60%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 61%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 62%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 63%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 64%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 65%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 66%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 67%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 68%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 69%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 70%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 71%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 72%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 73%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 74%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 75%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 76%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 77%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 78%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 79%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 80%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 81%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 82%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 83%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 84%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 85%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 86%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 87%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 88%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 89%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 90%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 91%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 92%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 93%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 94%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 95%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 96%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 97%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 98%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 99%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 100%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 101%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 102%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 103%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 104%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 105%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 106%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 107%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 108%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 109%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 110%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 111%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 112%positive => (-1 * (s IDimage_band_box_i) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -3 <= 0)%Z
    | 113%positive => (1 * (s IDimage_band_box_i) + -3 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_i) <= 0)%Z
    | 114%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -4 <= 0 /\ -1 * (s IDimage_band_box_i) + 1 <= 0)%Z
    | 115%positive => (-1 * (s IDimage_band_box_i) + 1 <= 0 /\ 1 * (s IDimage_band_box_i) + -4 <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 116%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_i) + -4 <= 0 /\ -1 * (s IDimage_band_box_i) + 1 <= 0)%Z
    | 117%positive => (-1 * (s IDimage_band_box_i) + 1 <= 0 /\ 1 * (s IDimage_band_box_i) + -4 <= 0 /\ -1 * (s IDimage_band_box_z) + 1 <= 0)%Z
    | 118%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 119%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 120%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 121%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 122%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 123%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 124%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 125%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 126%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 127%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 128%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 129%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 130%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 131%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 132%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 133%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 134%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 135%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 136%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 137%positive => (-1 * (s IDimage_band_box_z) <= 0 /\ 1 * (s IDimage_band_box_z) <= 0)%Z
    | 138%positive => (1 * (s IDimage_band_box_z) <= 0 /\ -1 * (s IDimage_band_box_z) <= 0)%Z
    | 139%positive => (-1 * (s IDimage_band_box_z) <= 0)%Z
    | 140%positive => (-1 * (s IDimage_band_box_z) <= 0)%Z
    | 141%positive => (-1 * (s IDimage_band_box_z) <= 0)%Z
    | 142%positive => (-1 * (s IDimage_band_box_z) <= 0)%Z
    | _ => False
  end.

Definition image_band_box_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 3%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 4%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 5%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 6%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 7%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 8%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 9%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 10%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 11%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 12%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 13%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 14%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 15%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 16%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 17%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 18%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 19%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 20%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 21%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 22%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 23%positive => ((4 # 1) + (s IDimage_band_box_z))%Q
    | 24%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 25%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 26%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 27%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 28%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 29%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 30%positive => (max0(4 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 31%positive => (max0(4 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 32%positive => (max0(4 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 33%positive => (max0(4 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 34%positive => ((s IDimage_band_box_z))%Q
    | 35%positive => (max0(4 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 36%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 37%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 38%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 39%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 40%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 41%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 42%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 43%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 44%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 45%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 46%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 47%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 48%positive => ((s IDimage_band_box_z)
                      + max0(4 - (s IDimage_band_box_i)))%Q
    | 49%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 50%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 51%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 52%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 53%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 54%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 55%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 56%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 57%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 58%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 59%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 60%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 61%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 62%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 63%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 64%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 65%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 66%positive => ((1 # 1) + (s IDimage_band_box_z)
                      + max0(3 - (s IDimage_band_box_i)))%Q
    | 67%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 68%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 69%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 70%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 71%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 72%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 73%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 74%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 75%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 76%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 77%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 78%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 79%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 80%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 81%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 82%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 83%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 84%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 85%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 86%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 87%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 88%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 89%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 90%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 91%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 92%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 93%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 94%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 95%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 96%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 97%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 98%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 99%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                      + max0((s IDimage_band_box_z)))%Q
    | 100%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 101%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 102%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 103%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 104%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 105%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 106%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 107%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 108%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 109%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 110%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 111%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 112%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 113%positive => ((1 # 1) + max0(3 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 114%positive => ((1 # 1) + max0(4 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 115%positive => ((1 # 1) + max0(4 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 116%positive => ((1 # 1) + max0(4 - (s IDimage_band_box_i))
                       + max0((s IDimage_band_box_z)))%Q
    | 117%positive => ((1 # 1) + max0(-1 + (s IDimage_band_box_z))
                       + max0(4 - (s IDimage_band_box_i)))%Q
    | 118%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 119%positive => ((4 # 1) + max0((s IDimage_band_box_z)))%Q
    | 120%positive => (0)%Q
    | 121%positive => (0)%Q
    | 122%positive => (0)%Q
    | 123%positive => (0)%Q
    | 124%positive => (0)%Q
    | 125%positive => (0)%Q
    | 126%positive => (0)%Q
    | 127%positive => (0)%Q
    | 128%positive => (0)%Q
    | 129%positive => (0)%Q
    | 130%positive => (0)%Q
    | 131%positive => (0)%Q
    | 132%positive => (0)%Q
    | 133%positive => (0)%Q
    | 134%positive => (0)%Q
    | 135%positive => ((s IDimage_band_box_z))%Q
    | 136%positive => ((s IDimage_band_box_z))%Q
    | 137%positive => ((s IDimage_band_box_z))%Q
    | 138%positive => ((s IDimage_band_box_z))%Q
    | 139%positive => ((s IDimage_band_box_z))%Q
    | 140%positive => ((s IDimage_band_box_z))%Q
    | 141%positive => ((s IDimage_band_box_z))%Q
    | 142%positive => ((s IDimage_band_box_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition image_band_box_hints (p : node) (s : state) := 
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
    | 22%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDimage_band_box_z)) (0))) (F_max0_ge_0 ((s IDimage_band_box_z)))]
    | 23%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDimage_band_box_z)) (0))) (F_max0_ge_0 ((s IDimage_band_box_z)))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDimage_band_box_i)) (3
                                                                    - (s IDimage_band_box_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDimage_band_box_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDimage_band_box_z))) (F_check_ge ((s IDimage_band_box_z)) (0))]
    | 34%positive => []
    | 35%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDimage_band_box_z))) (F_check_ge ((s IDimage_band_box_z)) (0))]
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDimage_band_box_i)) (1)]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDimage_band_box_i)) (1)]
    | 41%positive => []
    | 42%positive => []
    | 43%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDimage_band_box_i)) (1)]
    | 44%positive => []
    | 45%positive => []
    | 46%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDimage_band_box_i)) (1)]
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDimage_band_box_i)) (1)]
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
    | 63%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDimage_band_box_z)) (0))) (F_max0_ge_0 ((s IDimage_band_box_z)))]
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDimage_band_box_z)) (0))) (F_max0_ge_0 ((s IDimage_band_box_z)))]
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
    | 117%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDimage_band_box_z)) (0))) (F_max0_ge_0 ((s IDimage_band_box_z)));
                       (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDimage_band_box_z))) (F_check_ge (-1
                                                                    + (s IDimage_band_box_z)) (0))]
    | 118%positive => [(*-4 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDimage_band_box_z))) (F_check_ge (0) (0))]
    | 119%positive => [(*-4 0*) F_one;
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDimage_band_box_z))) (F_check_ge (0) (0))]
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
    | 132%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDimage_band_box_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDimage_band_box_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDimage_band_box_z)))]
    | 133%positive => []
    | 134%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDimage_band_box_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDimage_band_box_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDimage_band_box_z)))]
    | 135%positive => []
    | 136%positive => []
    | 137%positive => []
    | 138%positive => []
    | 139%positive => []
    | 140%positive => []
    | 141%positive => []
    | 142%positive => []
    | _ => []
  end.


Theorem image_band_box_ai_correct:
  forall s p' s', steps (g_start image_band_box) s (g_edges image_band_box) p' s' -> image_band_box_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem image_band_box_pot_correct:
  forall s p' s',
    steps (g_start image_band_box) s (g_edges image_band_box) p' s' ->
    (image_band_box_pot (g_start image_band_box) s >= image_band_box_pot p' s')%Q.
Proof.
  check_lp image_band_box_ai_correct image_band_box_hints.
Qed.

