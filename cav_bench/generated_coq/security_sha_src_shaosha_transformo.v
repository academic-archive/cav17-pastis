Require Import pasta.Pasta.

Notation IDsha_transform_z := 1%positive.
Notation IDsha_transform_A := 2%positive.
Notation IDsha_transform_B := 3%positive.
Notation IDsha_transform_C := 4%positive.
Notation IDsha_transform_D := 5%positive.
Notation IDsha_transform_E := 6%positive.
Notation IDsha_transform_i := 7%positive.
Notation IDsha_transform_sha_info_dref_off0_off0 := 8%positive.
Notation IDsha_transform_sha_info_dref_off0_off16 := 9%positive.
Notation IDsha_transform_sha_info_dref_off0_off24 := 10%positive.
Notation IDsha_transform_sha_info_dref_off0_off32 := 11%positive.
Notation IDsha_transform_sha_info_dref_off0_off8 := 12%positive.
Notation IDsha_transform_temp := 13%positive.
Notation IDsha_transform_sha_info := 14%positive.
Definition sha_transform : graph := {|
  g_start := 1%positive;
  g_end := 43%positive;
  g_edges := (1%positive,(AAssign IDsha_transform_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsha_transform_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) < (eval (ENum (16)) s))%Z)),103%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) >= (eval (ENum (16)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDsha_transform_i (Some (ENum (16)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) < (eval (ENum (80)) s))%Z)),96%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) >= (eval (ENum (80)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDsha_transform_A
             (Some (EVar IDsha_transform_sha_info_dref_off0_off0))),
             13%positive)::
             (13%positive,(AAssign IDsha_transform_B
             (Some (EVar IDsha_transform_sha_info_dref_off0_off8))),
             14%positive)::
             (14%positive,(AAssign IDsha_transform_C
             (Some (EVar IDsha_transform_sha_info_dref_off0_off16))),
             15%positive)::
             (15%positive,(AAssign IDsha_transform_D
             (Some (EVar IDsha_transform_sha_info_dref_off0_off24))),
             16%positive)::
             (16%positive,(AAssign IDsha_transform_E
             (Some (EVar IDsha_transform_sha_info_dref_off0_off32))),
             17%positive)::
             (17%positive,(AAssign IDsha_transform_i (Some (ENum (0)))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) < (eval (ENum (20)) s))%Z)),83%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) >= (eval (ENum (20)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDsha_transform_i (Some (ENum (20)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) < (eval (ENum (40)) s))%Z)),70%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) >= (eval (ENum (40)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDsha_transform_i (Some (ENum (40)))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) < (eval (ENum (60)) s))%Z)),57%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) >= (eval (ENum (60)) s))%Z)),31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AAssign IDsha_transform_i (Some (ENum (60)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) < (eval (ENum (80)) s))%Z)),44%positive)::
             (35%positive,(AGuard (fun s => ((eval (EVar IDsha_transform_i)
             s) >= (eval (ENum (80)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDsha_transform_sha_info_dref_off0_off0
             (Some (EAdd (EVar IDsha_transform_sha_info_dref_off0_off0)
             (EVar IDsha_transform_A)))),38%positive)::
             (38%positive,(AAssign IDsha_transform_sha_info_dref_off0_off8
             (Some (EAdd (EVar IDsha_transform_sha_info_dref_off0_off8)
             (EVar IDsha_transform_B)))),39%positive)::
             (39%positive,(AAssign IDsha_transform_sha_info_dref_off0_off16
             (Some (EAdd (EVar IDsha_transform_sha_info_dref_off0_off16)
             (EVar IDsha_transform_C)))),40%positive)::
             (40%positive,(AAssign IDsha_transform_sha_info_dref_off0_off24
             (Some (EAdd (EVar IDsha_transform_sha_info_dref_off0_off24)
             (EVar IDsha_transform_D)))),41%positive)::
             (41%positive,(AAssign IDsha_transform_sha_info_dref_off0_off32
             (Some (EAdd (EVar IDsha_transform_sha_info_dref_off0_off32)
             (EVar IDsha_transform_E)))),42%positive)::
             (42%positive,AWeaken,43%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDsha_transform_temp None),46%positive)::
             (46%positive,(AAssign IDsha_transform_E
             (Some (EVar IDsha_transform_D))),47%positive)::
             (47%positive,(AAssign IDsha_transform_D
             (Some (EVar IDsha_transform_C))),48%positive)::
             (48%positive,(AAssign IDsha_transform_C None),49%positive)::
             (49%positive,(AAssign IDsha_transform_B
             (Some (EVar IDsha_transform_A))),50%positive)::
             (50%positive,(AAssign IDsha_transform_A
             (Some (EVar IDsha_transform_temp))),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDsha_transform_i
             (Some (EAdd (EVar IDsha_transform_i) (ENum (1))))),53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDsha_transform_z (Some (EAdd (ENum (1))
             (EVar IDsha_transform_z)))),56%positive)::
             (56%positive,AWeaken,35%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AAssign IDsha_transform_temp None),59%positive)::
             (59%positive,(AAssign IDsha_transform_E
             (Some (EVar IDsha_transform_D))),60%positive)::
             (60%positive,(AAssign IDsha_transform_D
             (Some (EVar IDsha_transform_C))),61%positive)::
             (61%positive,(AAssign IDsha_transform_C None),62%positive)::
             (62%positive,(AAssign IDsha_transform_B
             (Some (EVar IDsha_transform_A))),63%positive)::
             (63%positive,(AAssign IDsha_transform_A
             (Some (EVar IDsha_transform_temp))),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDsha_transform_i
             (Some (EAdd (EVar IDsha_transform_i) (ENum (1))))),66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,(AAssign IDsha_transform_z (Some (EAdd (ENum (1))
             (EVar IDsha_transform_z)))),69%positive)::
             (69%positive,AWeaken,30%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,(AAssign IDsha_transform_temp None),72%positive)::
             (72%positive,(AAssign IDsha_transform_E
             (Some (EVar IDsha_transform_D))),73%positive)::
             (73%positive,(AAssign IDsha_transform_D
             (Some (EVar IDsha_transform_C))),74%positive)::
             (74%positive,(AAssign IDsha_transform_C None),75%positive)::
             (75%positive,(AAssign IDsha_transform_B
             (Some (EVar IDsha_transform_A))),76%positive)::
             (76%positive,(AAssign IDsha_transform_A
             (Some (EVar IDsha_transform_temp))),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,(AAssign IDsha_transform_i
             (Some (EAdd (EVar IDsha_transform_i) (ENum (1))))),79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,(AAssign IDsha_transform_z (Some (EAdd (ENum (1))
             (EVar IDsha_transform_z)))),82%positive)::
             (82%positive,AWeaken,25%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,(AAssign IDsha_transform_temp None),85%positive)::
             (85%positive,(AAssign IDsha_transform_E
             (Some (EVar IDsha_transform_D))),86%positive)::
             (86%positive,(AAssign IDsha_transform_D
             (Some (EVar IDsha_transform_C))),87%positive)::
             (87%positive,(AAssign IDsha_transform_C None),88%positive)::
             (88%positive,(AAssign IDsha_transform_B
             (Some (EVar IDsha_transform_A))),89%positive)::
             (89%positive,(AAssign IDsha_transform_A
             (Some (EVar IDsha_transform_temp))),90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,(AAssign IDsha_transform_i
             (Some (EAdd (EVar IDsha_transform_i) (ENum (1))))),92%positive)::
             (92%positive,ANone,93%positive)::
             (93%positive,ANone,94%positive)::
             (94%positive,(AAssign IDsha_transform_z (Some (EAdd (ENum (1))
             (EVar IDsha_transform_z)))),95%positive)::
             (95%positive,AWeaken,20%positive)::
             (96%positive,AWeaken,97%positive)::
             (97%positive,ANone,98%positive)::
             (98%positive,(AAssign IDsha_transform_i
             (Some (EAdd (EVar IDsha_transform_i) (ENum (1))))),99%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,(AAssign IDsha_transform_z (Some (EAdd (ENum (1))
             (EVar IDsha_transform_z)))),102%positive)::
             (102%positive,AWeaken,10%positive)::
             (103%positive,AWeaken,104%positive)::
             (104%positive,ANone,105%positive)::
             (105%positive,(AAssign IDsha_transform_i
             (Some (EAdd (EVar IDsha_transform_i) (ENum (1))))),106%positive)::
             (106%positive,ANone,107%positive)::
             (107%positive,ANone,108%positive)::
             (108%positive,(AAssign IDsha_transform_z (Some (EAdd (ENum (1))
             (EVar IDsha_transform_z)))),109%positive)::
             (109%positive,AWeaken,5%positive)::nil
|}.

Definition sha_transform_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 4%positive => (-1 * (s IDsha_transform_i) <= 0 /\ 1 * (s IDsha_transform_i) <= 0 /\ 1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0 /\ 1 * (s IDsha_transform_i) + -16 <= 0)%Z
    | 6%positive => (1 * (s IDsha_transform_i) + -16 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 16 <= 0)%Z
    | 7%positive => (-1 * (s IDsha_transform_i) + 16 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -16 <= 0)%Z
    | 8%positive => (-1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -16 <= 0 /\ -1 * (s IDsha_transform_i) + 16 <= 0)%Z
    | 9%positive => (-1 * (s IDsha_transform_i) + 16 <= 0 /\ 1 * (s IDsha_transform_i) + -16 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 10%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 16 <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 11%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 12%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 13%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 14%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 15%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 16%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 17%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 18%positive => (-1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 19%positive => (-1 * (s IDsha_transform_i) <= 0 /\ 1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 20%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0 /\ 1 * (s IDsha_transform_i) + -20 <= 0)%Z
    | 21%positive => (1 * (s IDsha_transform_i) + -20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0)%Z
    | 22%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -20 <= 0)%Z
    | 23%positive => (-1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -20 <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0)%Z
    | 24%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ 1 * (s IDsha_transform_i) + -20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 25%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0 /\ 1 * (s IDsha_transform_i) + -40 <= 0)%Z
    | 26%positive => (1 * (s IDsha_transform_i) + -40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0)%Z
    | 27%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -40 <= 0)%Z
    | 28%positive => (-1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -40 <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0)%Z
    | 29%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ 1 * (s IDsha_transform_i) + -40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 30%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0 /\ 1 * (s IDsha_transform_i) + -60 <= 0)%Z
    | 31%positive => (1 * (s IDsha_transform_i) + -60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0)%Z
    | 32%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -60 <= 0)%Z
    | 33%positive => (-1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -60 <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0)%Z
    | 34%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ 1 * (s IDsha_transform_i) + -60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 35%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 36%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 37%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 38%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 39%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 40%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 41%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 42%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 80 <= 0)%Z
    | 43%positive => (-1 * (s IDsha_transform_i) + 80 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 44%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 45%positive => (1 * (s IDsha_transform_i) + -79 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0)%Z
    | 46%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 47%positive => (1 * (s IDsha_transform_i) + -79 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0)%Z
    | 48%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 49%positive => (1 * (s IDsha_transform_i) + -79 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0)%Z
    | 50%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 51%positive => (1 * (s IDsha_transform_i) + -79 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 60 <= 0)%Z
    | 52%positive => (-1 * (s IDsha_transform_i) + 60 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 53%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 61 <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 54%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_i) + 61 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 55%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 61 <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 56%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_i) + 61 <= 0 /\ -1 * (s IDsha_transform_z) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -59 <= 0)%Z
    | 58%positive => (1 * (s IDsha_transform_i) + -59 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0)%Z
    | 59%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -59 <= 0)%Z
    | 60%positive => (1 * (s IDsha_transform_i) + -59 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0)%Z
    | 61%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -59 <= 0)%Z
    | 62%positive => (1 * (s IDsha_transform_i) + -59 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0)%Z
    | 63%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -59 <= 0)%Z
    | 64%positive => (1 * (s IDsha_transform_i) + -59 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 40 <= 0)%Z
    | 65%positive => (-1 * (s IDsha_transform_i) + 40 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -59 <= 0)%Z
    | 66%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 41 <= 0 /\ 1 * (s IDsha_transform_i) + -60 <= 0)%Z
    | 67%positive => (1 * (s IDsha_transform_i) + -60 <= 0 /\ -1 * (s IDsha_transform_i) + 41 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 68%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 41 <= 0 /\ 1 * (s IDsha_transform_i) + -60 <= 0)%Z
    | 69%positive => (1 * (s IDsha_transform_i) + -60 <= 0 /\ -1 * (s IDsha_transform_i) + 41 <= 0 /\ -1 * (s IDsha_transform_z) + 1 <= 0)%Z
    | 70%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -39 <= 0)%Z
    | 71%positive => (1 * (s IDsha_transform_i) + -39 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0)%Z
    | 72%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -39 <= 0)%Z
    | 73%positive => (1 * (s IDsha_transform_i) + -39 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0)%Z
    | 74%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -39 <= 0)%Z
    | 75%positive => (1 * (s IDsha_transform_i) + -39 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0)%Z
    | 76%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -39 <= 0)%Z
    | 77%positive => (1 * (s IDsha_transform_i) + -39 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 20 <= 0)%Z
    | 78%positive => (-1 * (s IDsha_transform_i) + 20 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -39 <= 0)%Z
    | 79%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 21 <= 0 /\ 1 * (s IDsha_transform_i) + -40 <= 0)%Z
    | 80%positive => (1 * (s IDsha_transform_i) + -40 <= 0 /\ -1 * (s IDsha_transform_i) + 21 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 81%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 21 <= 0 /\ 1 * (s IDsha_transform_i) + -40 <= 0)%Z
    | 82%positive => (1 * (s IDsha_transform_i) + -40 <= 0 /\ -1 * (s IDsha_transform_i) + 21 <= 0 /\ -1 * (s IDsha_transform_z) + 1 <= 0)%Z
    | 83%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -19 <= 0)%Z
    | 84%positive => (1 * (s IDsha_transform_i) + -19 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 85%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -19 <= 0)%Z
    | 86%positive => (1 * (s IDsha_transform_i) + -19 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 87%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -19 <= 0)%Z
    | 88%positive => (1 * (s IDsha_transform_i) + -19 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 89%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -19 <= 0)%Z
    | 90%positive => (1 * (s IDsha_transform_i) + -19 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 91%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -19 <= 0)%Z
    | 92%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ 1 * (s IDsha_transform_i) + -20 <= 0)%Z
    | 93%positive => (1 * (s IDsha_transform_i) + -20 <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 94%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ 1 * (s IDsha_transform_i) + -20 <= 0)%Z
    | 95%positive => (1 * (s IDsha_transform_i) + -20 <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ -1 * (s IDsha_transform_z) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDsha_transform_i) + 16 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 97%positive => (1 * (s IDsha_transform_i) + -79 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 16 <= 0)%Z
    | 98%positive => (-1 * (s IDsha_transform_i) + 16 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -79 <= 0)%Z
    | 99%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 17 <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 100%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_i) + 17 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 101%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 17 <= 0 /\ 1 * (s IDsha_transform_i) + -80 <= 0)%Z
    | 102%positive => (1 * (s IDsha_transform_i) + -80 <= 0 /\ -1 * (s IDsha_transform_i) + 17 <= 0 /\ -1 * (s IDsha_transform_z) + 1 <= 0)%Z
    | 103%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -15 <= 0)%Z
    | 104%positive => (1 * (s IDsha_transform_i) + -15 <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) <= 0)%Z
    | 105%positive => (-1 * (s IDsha_transform_i) <= 0 /\ -1 * (s IDsha_transform_z) <= 0 /\ 1 * (s IDsha_transform_i) + -15 <= 0)%Z
    | 106%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ 1 * (s IDsha_transform_i) + -16 <= 0)%Z
    | 107%positive => (1 * (s IDsha_transform_i) + -16 <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ -1 * (s IDsha_transform_z) <= 0)%Z
    | 108%positive => (-1 * (s IDsha_transform_z) <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ 1 * (s IDsha_transform_i) + -16 <= 0)%Z
    | 109%positive => (1 * (s IDsha_transform_i) + -16 <= 0 /\ -1 * (s IDsha_transform_i) + 1 <= 0 /\ -1 * (s IDsha_transform_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition sha_transform_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((160 # 1))%Q
    | 2%positive => ((160 # 1) + (s IDsha_transform_z))%Q
    | 3%positive => ((52 # 21) * (s IDsha_transform_i)
                     + (s IDsha_transform_z)
                     + max0(16 - (s IDsha_transform_i))
                     + (163 # 60) * max0(60 - (s IDsha_transform_i))
                     - (19 # 79) * max0(79 - (s IDsha_transform_i)))%Q
    | 4%positive => ((52 # 21) * (s IDsha_transform_i)
                     + (s IDsha_transform_z)
                     + max0(16 - (s IDsha_transform_i))
                     + (163 # 60) * max0(60 - (s IDsha_transform_i))
                     - (19 # 79) * max0(79 - (s IDsha_transform_i)))%Q
    | 5%positive => (-(19 # 1) + (163 # 60) * (s IDsha_transform_i)
                     + (s IDsha_transform_z)
                     + max0(16 - (s IDsha_transform_i))
                     + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | 6%positive => (-(19 # 1) + (163 # 60) * (s IDsha_transform_i)
                     + (s IDsha_transform_z)
                     + max0(16 - (s IDsha_transform_i))
                     + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | 7%positive => ((144 # 1) + (s IDsha_transform_z))%Q
    | 8%positive => ((80 # 1) + (s IDsha_transform_z)
                     + max0(80 - (s IDsha_transform_i)))%Q
    | 9%positive => ((80 # 1) + (s IDsha_transform_z)
                     + max0(80 - (s IDsha_transform_i)))%Q
    | 10%positive => ((80 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 11%positive => ((80 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 12%positive => ((80 # 1) + (s IDsha_transform_z))%Q
    | 13%positive => ((80 # 1) + (s IDsha_transform_z))%Q
    | 14%positive => ((80 # 1) + (s IDsha_transform_z))%Q
    | 15%positive => ((80 # 1) + (s IDsha_transform_z))%Q
    | 16%positive => ((80 # 1) + (s IDsha_transform_z))%Q
    | 17%positive => ((80 # 1) + (s IDsha_transform_z))%Q
    | 18%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 19%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 20%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 21%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 22%positive => ((60 # 1) + (s IDsha_transform_z))%Q
    | 23%positive => ((2 # 3) * (s IDsha_transform_i) + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 24%positive => ((2 # 3) * (s IDsha_transform_i) + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 25%positive => ((2 # 3) * (s IDsha_transform_i) + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 26%positive => ((2 # 3) * (s IDsha_transform_i) + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 27%positive => ((40 # 1) + (s IDsha_transform_z))%Q
    | 28%positive => ((20 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 29%positive => ((20 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 30%positive => ((20 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 31%positive => ((20 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 32%positive => ((20 # 1) + (s IDsha_transform_z))%Q
    | 33%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 34%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 35%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 36%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 37%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 38%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 39%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 40%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 41%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 42%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 43%positive => ((s IDsha_transform_z))%Q
    | 44%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 45%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 46%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 47%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 48%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 49%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 50%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 51%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 52%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 53%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 54%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 55%positive => ((1 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 56%positive => ((s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 57%positive => ((20 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 58%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 59%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 60%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 61%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 62%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 63%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 64%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 65%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(59 - (s IDsha_transform_i)))%Q
    | 66%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 67%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 68%positive => ((21 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 69%positive => ((20 # 1) + (s IDsha_transform_z)
                      + max0(60 - (s IDsha_transform_i)))%Q
    | 70%positive => ((2 # 3) * (s IDsha_transform_i) + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 71%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 72%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 73%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 74%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 75%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 76%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 77%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 78%positive => ((5 # 3) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(39 - (s IDsha_transform_i))
                      + (2 # 3) * max0(59 - (s IDsha_transform_i)))%Q
    | 79%positive => ((1 # 1) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 80%positive => ((1 # 1) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 81%positive => ((1 # 1) + (2 # 3) * (s IDsha_transform_i)
                      + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 82%positive => ((2 # 3) * (s IDsha_transform_i) + (s IDsha_transform_z)
                      + max0(40 - (s IDsha_transform_i))
                      + (2 # 3) * max0(60 - (s IDsha_transform_i)))%Q
    | 83%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 84%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 85%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 86%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 87%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 88%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 89%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 90%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 91%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 92%positive => ((81 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 93%positive => ((81 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 94%positive => ((81 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 95%positive => ((80 # 1) - (s IDsha_transform_i)
                      + (s IDsha_transform_z))%Q
    | 96%positive => ((80 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 97%positive => ((81 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 98%positive => ((81 # 1) + (s IDsha_transform_z)
                      + max0(79 - (s IDsha_transform_i)))%Q
    | 99%positive => ((81 # 1) + (s IDsha_transform_z)
                      + max0(80 - (s IDsha_transform_i)))%Q
    | 100%positive => ((81 # 1) + (s IDsha_transform_z)
                       + max0(80 - (s IDsha_transform_i)))%Q
    | 101%positive => ((81 # 1) + (s IDsha_transform_z)
                       + max0(80 - (s IDsha_transform_i)))%Q
    | 102%positive => ((80 # 1) + (s IDsha_transform_z)
                       + max0(80 - (s IDsha_transform_i)))%Q
    | 103%positive => (-(19 # 1) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(16 - (s IDsha_transform_i))
                       + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | 104%positive => (-(917 # 60) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(15 - (s IDsha_transform_i))
                       + (163 # 60) * max0(59 - (s IDsha_transform_i)))%Q
    | 105%positive => (-(917 # 60) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(15 - (s IDsha_transform_i))
                       + (163 # 60) * max0(59 - (s IDsha_transform_i)))%Q
    | 106%positive => (-(18 # 1) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(16 - (s IDsha_transform_i))
                       + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | 107%positive => (-(18 # 1) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(16 - (s IDsha_transform_i))
                       + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | 108%positive => (-(18 # 1) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(16 - (s IDsha_transform_i))
                       + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | 109%positive => (-(19 # 1) + (163 # 60) * (s IDsha_transform_i)
                       + (s IDsha_transform_z)
                       + max0(16 - (s IDsha_transform_i))
                       + (163 # 60) * max0(60 - (s IDsha_transform_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition sha_transform_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-0.240506 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (79
                                                                    - (s IDsha_transform_i)) (0))) (F_max0_ge_0 (79
                                                                    - (s IDsha_transform_i)))]
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                            - (s IDsha_transform_i)) (15
                                                                    - (s IDsha_transform_i)));
                     (*-1 0*) F_max0_ge_0 (15 - (s IDsha_transform_i));
                     (*-2.71667 0*) F_binom_monotonic 1 (F_max0_ge_arg (60
                                                                    - (s IDsha_transform_i))) (F_check_ge (60
                                                                    - (s IDsha_transform_i)) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (80
                                                             - (s IDsha_transform_i)) (79
                                                                    - (s IDsha_transform_i)));
                      (*-1 0*) F_max0_ge_0 (79 - (s IDsha_transform_i))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (20
                                                             - (s IDsha_transform_i)) (19
                                                                    - (s IDsha_transform_i)));
                      (*-1 0*) F_max0_ge_0 (19 - (s IDsha_transform_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDsha_transform_i)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDsha_transform_i)))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (40
                                                             - (s IDsha_transform_i)) (39
                                                                    - (s IDsha_transform_i)));
                      (*-1 0*) F_max0_ge_0 (39 - (s IDsha_transform_i));
                      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_arg (60
                                                                    - (s IDsha_transform_i))) (F_check_ge (60
                                                                    - (s IDsha_transform_i)) (0))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (60
                                                             - (s IDsha_transform_i)) (59
                                                                    - (s IDsha_transform_i)));
                      (*-1 0*) F_max0_ge_0 (59 - (s IDsha_transform_i))]
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
    | 42%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (80
                                                             - (s IDsha_transform_i)) (79
                                                                    - (s IDsha_transform_i)));
                      (*-1 0*) F_max0_ge_0 (79 - (s IDsha_transform_i))]
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_max0_pre_decrement (80
                                                     - (s IDsha_transform_i)) (1)]
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
    | 57%positive => [(*-1 0*) F_max0_pre_decrement (60
                                                     - (s IDsha_transform_i)) (1)]
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
    | 70%positive => [(*-1 0*) F_max0_pre_decrement (40
                                                     - (s IDsha_transform_i)) (1);
                      (*0 0.666667*) F_max0_pre_decrement (60
                                                           - (s IDsha_transform_i)) (1)]
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
    | 96%positive => [(*-1 0*) F_max0_pre_decrement (80
                                                     - (s IDsha_transform_i)) (1)]
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => [(*-1 0*) F_max0_pre_decrement (16
                                                      - (s IDsha_transform_i)) (1);
                       (*-2.71667 0*) F_max0_pre_decrement (60
                                                            - (s IDsha_transform_i)) (1)]
    | 104%positive => []
    | 105%positive => []
    | 106%positive => []
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | _ => []
  end.


Theorem sha_transform_ai_correct:
  forall s p' s', steps (g_start sha_transform) s (g_edges sha_transform) p' s' -> sha_transform_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem sha_transform_pot_correct:
  forall s p' s',
    steps (g_start sha_transform) s (g_edges sha_transform) p' s' ->
    (sha_transform_pot (g_start sha_transform) s >= sha_transform_pot p' s')%Q.
Proof.
  check_lp sha_transform_ai_correct sha_transform_hints.
Qed.

