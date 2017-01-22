Require Import pasta.Pasta.

Notation IDfill_buffer_resample_z := 1%positive.
Notation IDfill_buffer_resample__tmp := 2%positive.
Notation IDfill_buffer_resample__tmp1 := 3%positive.
Notation IDfill_buffer_resample__tmp2 := 4%positive.
Notation IDfill_buffer_resample_gfp_dref_off168 := 5%positive.
Notation IDfill_buffer_resample_i := 6%positive.
Notation IDfill_buffer_resample_j := 7%positive.
Notation IDfill_buffer_resample_k := 8%positive.
Notation IDfill_buffer_resample_linear := 9%positive.
Notation IDfill_buffer_resample_num_used_dref := 10%positive.
Notation IDfill_buffer_resample_value := 11%positive.
Notation IDfill_buffer_resample_y0 := 12%positive.
Notation IDfill_buffer_resample_y1 := 13%positive.
Notation IDfill_buffer_resample_y2 := 14%positive.
Notation IDfill_buffer_resample_y3 := 15%positive.
Notation IDfill_buffer_resample_ch := 16%positive.
Notation IDfill_buffer_resample_desired_len := 17%positive.
Notation IDfill_buffer_resample_gfp := 18%positive.
Notation IDfill_buffer_resample_inbuf := 19%positive.
Notation IDfill_buffer_resample_len := 20%positive.
Notation IDfill_buffer_resample_num_used := 21%positive.
Notation IDfill_buffer_resample_outbuf := 22%positive.
Definition fill_buffer_resample : graph := {|
  g_start := 1%positive;
  g_end := 90%positive;
  g_edges := (1%positive,(AAssign IDfill_buffer_resample_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDfill_buffer_resample__tmp
             (Some (EVar IDfill_buffer_resample_desired_len))),3%positive)::
             (3%positive,(AAssign IDfill_buffer_resample__tmp1
             (Some (EVar IDfill_buffer_resample_len))),4%positive)::
             (4%positive,(AAssign IDfill_buffer_resample__tmp2
             (Some (EVar IDfill_buffer_resample_ch))),5%positive)::
             (5%positive,(AAssign IDfill_buffer_resample_j
             (Some (ENum (0)))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_gfp_dref_off168)
             s) = (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_gfp_dref_off168)
             s) <> (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,14%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,13%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,14%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_gfp_dref_off168)
             s) <> (eval (ENum (0)) s))%Z)),16%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_gfp_dref_off168)
             s) = (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,18%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDfill_buffer_resample_linear None),
             19%positive)::
             (19%positive,(AAssign IDfill_buffer_resample_k
             (Some (ENum (0)))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_k) s) <
             (eval (EVar IDfill_buffer_resample__tmp) s))%Z)),24%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_k) s) >=
             (eval (EVar IDfill_buffer_resample__tmp) s))%Z)),23%positive)::
             (23%positive,AWeaken,79%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDfill_buffer_resample_j None),
             26%positive)::(26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDfill_buffer_resample_j)
             (ENum (2))) s) >= (eval (EVar IDfill_buffer_resample__tmp1)
             s))%Z)),76%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDfill_buffer_resample_j)
             (ENum (2))) s) < (eval (EVar IDfill_buffer_resample__tmp1)
             s))%Z)),28%positive)::(28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_j) s) <
             (eval (ENum (0)) s))%Z)),32%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_j) s) >=
             (eval (ENum (0)) s))%Z)),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,34%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDfill_buffer_resample_y1 None),
             35%positive)::(35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard (fun s => ((eval (EAdd (ENum (1))
             (EVar IDfill_buffer_resample_j)) s) < (eval (ENum (0)) s))%Z)),
             39%positive)::
             (36%positive,(AGuard (fun s => ((eval (EAdd (ENum (1))
             (EVar IDfill_buffer_resample_j)) s) >= (eval (ENum (0)) s))%Z)),
             37%positive)::(37%positive,AWeaken,38%positive)::
             (38%positive,ANone,41%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDfill_buffer_resample_y2 None),
             42%positive)::(42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_linear) s) <>
             (eval (ENum (0)) s))%Z)),68%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_linear) s) =
             (eval (ENum (0)) s))%Z)),44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDfill_buffer_resample_j)
             (ENum (1))) s) < (eval (ENum (0)) s))%Z)),48%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDfill_buffer_resample_j)
             (ENum (1))) s) >= (eval (ENum (0)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,50%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDfill_buffer_resample_y0 None),
             51%positive)::(51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDfill_buffer_resample_j)
             (ENum (2))) s) < (eval (ENum (0)) s))%Z)),55%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDfill_buffer_resample_j)
             (ENum (2))) s) >= (eval (ENum (0)) s))%Z)),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,57%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDfill_buffer_resample_y3 None),
             58%positive)::
             (58%positive,(AAssign IDfill_buffer_resample_value None),
             59%positive)::(59%positive,AWeaken,60%positive)::
             (60%positive,ANone,66%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,64%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,ANone,65%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,67%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,ANone,70%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,(AAssign IDfill_buffer_resample_k
             (Some (EAdd (EVar IDfill_buffer_resample_k) (ENum (1))))),
             72%positive)::(72%positive,ANone,73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,(AAssign IDfill_buffer_resample_z
             (Some (EAdd (ENum (1)) (EVar IDfill_buffer_resample_z)))),
             75%positive)::(75%positive,AWeaken,22%positive)::
             (76%positive,AWeaken,77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample__tmp1) s) <
             (eval (EAdd (EVar IDfill_buffer_resample_j) (ENum (2))) s))%Z)),
             82%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample__tmp1) s) >=
             (eval (EAdd (EVar IDfill_buffer_resample_j) (ENum (2))) s))%Z)),
             80%positive)::(80%positive,AWeaken,81%positive)::
             (81%positive,ANone,84%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,ANone,84%positive)::
             (84%positive,(AAssign IDfill_buffer_resample_num_used_dref
             None),85%positive)::
             (85%positive,(AAssign IDfill_buffer_resample_i
             (Some (ENum (0)))),86%positive)::
             (86%positive,ANone,87%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_i) s) <
             (eval (ENum (5)) s))%Z)),91%positive)::
             (88%positive,(AGuard
             (fun s => ((eval (EVar IDfill_buffer_resample_i) s) >=
             (eval (ENum (5)) s))%Z)),89%positive)::
             (89%positive,AWeaken,90%positive)::
             (91%positive,AWeaken,92%positive)::
             (92%positive,ANone,93%positive)::
             (93%positive,(AAssign IDfill_buffer_resample_i
             (Some (EAdd (EVar IDfill_buffer_resample_i) (ENum (1))))),
             94%positive)::(94%positive,ANone,95%positive)::
             (95%positive,ANone,96%positive)::
             (96%positive,(AAssign IDfill_buffer_resample_z
             (Some (EAdd (ENum (1)) (EVar IDfill_buffer_resample_z)))),
             97%positive)::(97%positive,AWeaken,88%positive)::nil
|}.

Definition fill_buffer_resample_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 4%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 6%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0)%Z
    | 7%positive => (-1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 8%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0)%Z
    | 9%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ -1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0)%Z
    | 10%positive => (-1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ 1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 11%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ -1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0)%Z
    | 12%positive => (-1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ 1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 13%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ -1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0)%Z
    | 14%positive => (-1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 15%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0 /\ -1 * (s IDfill_buffer_resample_gfp_dref_off168) <= 0)%Z
    | 16%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0)%Z
    | 17%positive => (-1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 18%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0)%Z
    | 19%positive => (-1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 20%positive => (1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 21%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ 1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 22%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 23%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample__tmp)+ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 24%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 26%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 28%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 29%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 30%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample_j) <= 0)%Z
    | 31%positive => (-1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 32%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_j) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDfill_buffer_resample_j) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 34%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 35%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 36%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 37%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_j) + -1 <= 0)%Z
    | 38%positive => (-1 * (s IDfill_buffer_resample_j) + -1 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 39%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ 1 * (s IDfill_buffer_resample_j) + 2 <= 0)%Z
    | 40%positive => (1 * (s IDfill_buffer_resample_j) + 2 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 41%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 42%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 43%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 44%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 45%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 46%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_j) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDfill_buffer_resample_j) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 48%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_j) <= 0)%Z
    | 49%positive => (1 * (s IDfill_buffer_resample_j) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 50%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 51%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 52%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 53%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_j) + -2 <= 0)%Z
    | 54%positive => (-1 * (s IDfill_buffer_resample_j) + -2 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 55%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 56%positive => (1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 57%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 58%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 59%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 60%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 61%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 62%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 63%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 64%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 65%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 66%positive => (-1 * (s IDfill_buffer_resample_linear) <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 67%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ 1 * (s IDfill_buffer_resample_linear) <= 0 /\ -1 * (s IDfill_buffer_resample_linear) <= 0)%Z
    | 68%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 69%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 70%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0)%Z
    | 71%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 72%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_k) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 74%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_k) + 1 <= 0)%Z
    | 75%positive => (-1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 3 <= 0 /\ -1 * (s IDfill_buffer_resample_z) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ 1 * (s IDfill_buffer_resample__tmp1)+ -1 * (s IDfill_buffer_resample_j) + -2 <= 0)%Z
    | 77%positive => (1 * (s IDfill_buffer_resample__tmp1)+ -1 * (s IDfill_buffer_resample_j) + -2 <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 78%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp)+ 1 * (s IDfill_buffer_resample_k) + 1 <= 0 /\ 1 * (s IDfill_buffer_resample__tmp1)+ -1 * (s IDfill_buffer_resample_j) + -2 <= 0)%Z
    | 79%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 80%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 2 <= 0)%Z
    | 81%positive => (-1 * (s IDfill_buffer_resample__tmp1)+ 1 * (s IDfill_buffer_resample_j) + 2 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 82%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample__tmp1)+ -1 * (s IDfill_buffer_resample_j) + -1 <= 0)%Z
    | 83%positive => (1 * (s IDfill_buffer_resample__tmp1)+ -1 * (s IDfill_buffer_resample_j) + -1 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 84%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 85%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 86%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_i) <= 0 /\ -1 * (s IDfill_buffer_resample_i) <= 0)%Z
    | 87%positive => (-1 * (s IDfill_buffer_resample_i) <= 0 /\ 1 * (s IDfill_buffer_resample_i) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 88%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_i) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ 1 * (s IDfill_buffer_resample_i) + -5 <= 0)%Z
    | 89%positive => (1 * (s IDfill_buffer_resample_i) + -5 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_i) + 5 <= 0)%Z
    | 90%positive => (-1 * (s IDfill_buffer_resample_i) + 5 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ 1 * (s IDfill_buffer_resample_i) + -5 <= 0)%Z
    | 91%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_i) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_i) + -4 <= 0)%Z
    | 92%positive => (1 * (s IDfill_buffer_resample_i) + -4 <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_i) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0)%Z
    | 93%positive => (-1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_i) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0 /\ 1 * (s IDfill_buffer_resample_i) + -4 <= 0)%Z
    | 94%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_i) + 1 <= 0 /\ 1 * (s IDfill_buffer_resample_i) + -5 <= 0)%Z
    | 95%positive => (1 * (s IDfill_buffer_resample_i) + -5 <= 0 /\ -1 * (s IDfill_buffer_resample_i) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) <= 0)%Z
    | 96%positive => (-1 * (s IDfill_buffer_resample_z) <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_i) + 1 <= 0 /\ 1 * (s IDfill_buffer_resample_i) + -5 <= 0)%Z
    | 97%positive => (1 * (s IDfill_buffer_resample_i) + -5 <= 0 /\ -1 * (s IDfill_buffer_resample_i) + 1 <= 0 /\ -1 * (s IDfill_buffer_resample_k) <= 0 /\ -1 * (s IDfill_buffer_resample_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition fill_buffer_resample_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((5 # 1) + max0((s IDfill_buffer_resample_desired_len)))%Q
    | 2%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                     + max0((s IDfill_buffer_resample_desired_len)))%Q
    | 3%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 4%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 5%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 6%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                     + (s IDfill_buffer_resample_z)
                     + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 7%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                     + (s IDfill_buffer_resample_z)
                     + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 8%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                     + (s IDfill_buffer_resample_z)
                     + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 9%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                     + (s IDfill_buffer_resample_z)
                     + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                     + max0((s IDfill_buffer_resample__tmp)))%Q
    | 10%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 11%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 12%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 13%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 14%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 15%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 16%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 17%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 18%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 19%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)))%Q
    | 20%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 21%positive => (-(5 # 1) * (s IDfill_buffer_resample_j)
                      + (s IDfill_buffer_resample_z)
                      + (5 # 1) * max0(1 + (s IDfill_buffer_resample_j))
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 22%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 23%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 24%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 25%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 26%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 27%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 28%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 29%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 30%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 31%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 32%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 33%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 34%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 35%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 36%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 37%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 38%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 39%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 40%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 41%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 42%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 43%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 44%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 45%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 46%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 47%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 48%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 49%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 50%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 51%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 52%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 53%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 54%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 55%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 56%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 57%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 58%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 59%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 60%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 61%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 62%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 63%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 64%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 65%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 66%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 67%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 68%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 69%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 70%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 71%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0(-1 + (s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 72%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 73%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 74%positive => ((6 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 75%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 76%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 77%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 78%positive => ((5 # 1) + (s IDfill_buffer_resample_z)
                      + max0((s IDfill_buffer_resample__tmp)
                             - (s IDfill_buffer_resample_k)))%Q
    | 79%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 80%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 81%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 82%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 83%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 84%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 85%positive => ((5 # 1) + (s IDfill_buffer_resample_z))%Q
    | 86%positive => ((s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 87%positive => ((s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 88%positive => ((s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 89%positive => ((s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 90%positive => ((s IDfill_buffer_resample_z))%Q
    | 91%positive => ((s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 92%positive => ((1 # 1) + (s IDfill_buffer_resample_z)
                      + max0(4 - (s IDfill_buffer_resample_i)))%Q
    | 93%positive => ((1 # 1) + (s IDfill_buffer_resample_z)
                      + max0(4 - (s IDfill_buffer_resample_i)))%Q
    | 94%positive => ((1 # 1) + (s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 95%positive => ((1 # 1) + (s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 96%positive => ((1 # 1) + (s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | 97%positive => ((s IDfill_buffer_resample_z)
                      + max0(5 - (s IDfill_buffer_resample_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fill_buffer_resample_hints (p : node) (s : state) := 
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
    | 21%positive => [(*-5 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDfill_buffer_resample_j))) (F_check_ge (1
                                                                    + (s IDfill_buffer_resample_j)) (0))]
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfill_buffer_resample__tmp)
                                                             - (s IDfill_buffer_resample_k)) (-1
                                                                    + (s IDfill_buffer_resample__tmp)
                                                                    - (s IDfill_buffer_resample_k)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDfill_buffer_resample__tmp)
                                            - (s IDfill_buffer_resample_k))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*0 1*) F_max0_pre_decrement ((s IDfill_buffer_resample__tmp)
                                                    - (s IDfill_buffer_resample_k)) (1)]
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
    | 78%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDfill_buffer_resample__tmp)
                                                     - (s IDfill_buffer_resample_k)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDfill_buffer_resample__tmp)
                                            - (s IDfill_buffer_resample_k))]
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
    | 89%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (5
                                                             - (s IDfill_buffer_resample_i)) (4
                                                                    - (s IDfill_buffer_resample_i)));
                      (*-1 0*) F_max0_ge_0 (4 - (s IDfill_buffer_resample_i))]
    | 90%positive => []
    | 91%positive => [(*-1 0*) F_max0_pre_decrement (5
                                                     - (s IDfill_buffer_resample_i)) (1)]
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | _ => []
  end.


Theorem fill_buffer_resample_ai_correct:
  forall s p' s', steps (g_start fill_buffer_resample) s (g_edges fill_buffer_resample) p' s' -> fill_buffer_resample_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fill_buffer_resample_pot_correct:
  forall s p' s',
    steps (g_start fill_buffer_resample) s (g_edges fill_buffer_resample) p' s' ->
    (fill_buffer_resample_pot (g_start fill_buffer_resample) s >= fill_buffer_resample_pot p' s')%Q.
Proof.
  check_lp fill_buffer_resample_ai_correct fill_buffer_resample_hints.
Qed.

