Require Import pasta.Pasta.

Notation IDadpcm_coder_z := 1%positive.
Notation IDadpcm_coder__tmp := 2%positive.
Notation IDadpcm_coder_bufferstep := 3%positive.
Notation IDadpcm_coder_delta := 4%positive.
Notation IDadpcm_coder_diff := 5%positive.
Notation IDadpcm_coder_index := 6%positive.
Notation IDadpcm_coder_outputbuffer := 7%positive.
Notation IDadpcm_coder_sign := 8%positive.
Notation IDadpcm_coder_state_dref_off0 := 9%positive.
Notation IDadpcm_coder_state_dref_off2 := 10%positive.
Notation IDadpcm_coder_step := 11%positive.
Notation IDadpcm_coder_val := 12%positive.
Notation IDadpcm_coder_valpred := 13%positive.
Notation IDadpcm_coder_vpdiff := 14%positive.
Notation IDadpcm_coder_indata := 15%positive.
Notation IDadpcm_coder_len := 16%positive.
Notation IDadpcm_coder_outdata := 17%positive.
Notation IDadpcm_coder_state := 18%positive.
Definition adpcm_coder : graph := {|
  g_start := 1%positive;
  g_end := 19%positive;
  g_edges := (1%positive,(AAssign IDadpcm_coder_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDadpcm_coder__tmp
             (Some (EVar IDadpcm_coder_len))),3%positive)::
             (3%positive,(AAssign IDadpcm_coder_outputbuffer
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDadpcm_coder_valpred
             (Some (EVar IDadpcm_coder_state_dref_off0))),5%positive)::
             (5%positive,(AAssign IDadpcm_coder_index
             (Some (EVar IDadpcm_coder_state_dref_off2))),6%positive)::
             (6%positive,(AAssign IDadpcm_coder_step None),7%positive)::
             (7%positive,(AAssign IDadpcm_coder_bufferstep
             (Some (ENum (1)))),8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder__tmp)
             s) > (eval (ENum (0)) s))%Z)),20%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder__tmp)
             s) <= (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_coder_bufferstep) s) <>
             (eval (ENum (0)) s))%Z)),15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_coder_bufferstep) s) =
             (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,16%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDadpcm_coder_state_dref_off0
             (Some (EVar IDadpcm_coder_valpred))),17%positive)::
             (17%positive,(AAssign IDadpcm_coder_state_dref_off2
             (Some (EVar IDadpcm_coder_index))),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDadpcm_coder_val None),22%positive)::
             (22%positive,(AAssign IDadpcm_coder_diff
             (Some (ESub (EVar IDadpcm_coder_val)
             (EVar IDadpcm_coder_valpred)))),23%positive)::
             (23%positive,(AAssign IDadpcm_coder_sign None),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_sign)
             s) <> (eval (ENum (0)) s))%Z)),27%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_sign)
             s) = (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,30%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDadpcm_coder_diff (Some (ESub (ENum (0))
             (EVar IDadpcm_coder_diff)))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDadpcm_coder_delta (Some (ENum (0)))),
             31%positive)::
             (31%positive,(AAssign IDadpcm_coder_vpdiff None),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_diff)
             s) >= (eval (EVar IDadpcm_coder_step) s))%Z)),35%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_diff)
             s) < (eval (EVar IDadpcm_coder_step) s))%Z)),34%positive)::
             (34%positive,AWeaken,40%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDadpcm_coder_delta (Some (ENum (4)))),
             37%positive)::
             (37%positive,(AAssign IDadpcm_coder_diff
             (Some (ESub (EVar IDadpcm_coder_diff)
             (EVar IDadpcm_coder_step)))),38%positive)::
             (38%positive,(AAssign IDadpcm_coder_vpdiff
             (Some (EAdd (EVar IDadpcm_coder_vpdiff)
             (EVar IDadpcm_coder_step)))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDadpcm_coder_step None),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_diff)
             s) >= (eval (EVar IDadpcm_coder_step) s))%Z)),44%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_diff)
             s) < (eval (EVar IDadpcm_coder_step) s))%Z)),43%positive)::
             (43%positive,AWeaken,49%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDadpcm_coder_delta None),46%positive)::
             (46%positive,(AAssign IDadpcm_coder_diff
             (Some (ESub (EVar IDadpcm_coder_diff)
             (EVar IDadpcm_coder_step)))),47%positive)::
             (47%positive,(AAssign IDadpcm_coder_vpdiff
             (Some (EAdd (EVar IDadpcm_coder_vpdiff)
             (EVar IDadpcm_coder_step)))),48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDadpcm_coder_step None),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_diff)
             s) >= (eval (EVar IDadpcm_coder_step) s))%Z)),53%positive)::
             (51%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_diff)
             s) < (eval (EVar IDadpcm_coder_step) s))%Z)),52%positive)::
             (52%positive,AWeaken,58%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AAssign IDadpcm_coder_delta None),55%positive)::
             (55%positive,(AAssign IDadpcm_coder_vpdiff
             (Some (EAdd (EVar IDadpcm_coder_vpdiff)
             (EVar IDadpcm_coder_step)))),56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_sign)
             s) <> (eval (ENum (0)) s))%Z)),63%positive)::
             (58%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_sign)
             s) = (eval (ENum (0)) s))%Z)),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AAssign IDadpcm_coder_valpred
             (Some (EAdd (EVar IDadpcm_coder_valpred)
             (EVar IDadpcm_coder_vpdiff)))),61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,AWeaken,67%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDadpcm_coder_valpred
             (Some (ESub (EVar IDadpcm_coder_valpred)
             (EVar IDadpcm_coder_vpdiff)))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,73%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,70%positive)::
             (69%positive,ANone,72%positive)::
             (70%positive,(AAssign IDadpcm_coder_valpred None),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,75%positive)::
             (73%positive,(AAssign IDadpcm_coder_valpred None),74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDadpcm_coder_delta None),76%positive)::
             (76%positive,(AAssign IDadpcm_coder_index None),77%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_index)
             s) < (eval (ENum (0)) s))%Z)),80%positive)::
             (78%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_index)
             s) >= (eval (ENum (0)) s))%Z)),79%positive)::
             (79%positive,AWeaken,84%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,(AAssign IDadpcm_coder_index (Some (ENum (0)))),
             82%positive)::(82%positive,ANone,83%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_index)
             s) > (eval (ENum (88)) s))%Z)),86%positive)::
             (84%positive,(AGuard (fun s => ((eval (EVar IDadpcm_coder_index)
             s) <= (eval (ENum (88)) s))%Z)),85%positive)::
             (85%positive,AWeaken,89%positive)::
             (86%positive,AWeaken,87%positive)::
             (87%positive,(AAssign IDadpcm_coder_index (Some (ENum (88)))),
             88%positive)::(88%positive,ANone,89%positive)::
             (89%positive,(AAssign IDadpcm_coder_step None),90%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_coder_bufferstep) s) <>
             (eval (ENum (0)) s))%Z)),94%positive)::
             (91%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_coder_bufferstep) s) =
             (eval (ENum (0)) s))%Z)),92%positive)::
             (92%positive,AWeaken,93%positive)::
             (93%positive,ANone,97%positive)::
             (94%positive,AWeaken,95%positive)::
             (95%positive,(AAssign IDadpcm_coder_outputbuffer None),
             96%positive)::(96%positive,ANone,97%positive)::
             (97%positive,(AAssign IDadpcm_coder_bufferstep None),
             98%positive)::(98%positive,ANone,99%positive)::
             (99%positive,(AAssign IDadpcm_coder__tmp
             (Some (EAdd (EVar IDadpcm_coder__tmp) (ENum (-1))))),
             100%positive)::(100%positive,ANone,101%positive)::
             (101%positive,ANone,102%positive)::
             (102%positive,(AAssign IDadpcm_coder_z (Some (EAdd (ENum (1))
             (EVar IDadpcm_coder_z)))),103%positive)::
             (103%positive,AWeaken,10%positive)::nil
|}.

Definition adpcm_coder_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 3%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_z) <= 0)%Z
    | 4%positive => (1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ -1 * (s IDadpcm_coder_outputbuffer) <= 0)%Z
    | 5%positive => (-1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ 1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_z) <= 0)%Z
    | 6%positive => (1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ -1 * (s IDadpcm_coder_outputbuffer) <= 0)%Z
    | 7%positive => (-1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ 1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_z) <= 0)%Z
    | 8%positive => (1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ -1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ 1 * (s IDadpcm_coder_bufferstep) + -1 <= 0 /\ -1 * (s IDadpcm_coder_bufferstep) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDadpcm_coder_bufferstep) + 1 <= 0 /\ 1 * (s IDadpcm_coder_bufferstep) + -1 <= 0 /\ -1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ 1 * (s IDadpcm_coder_outputbuffer) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_z) <= 0)%Z
    | 10%positive => (-1 * (s IDadpcm_coder_z) <= 0)%Z
    | 11%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder__tmp) <= 0)%Z
    | 12%positive => (1 * (s IDadpcm_coder__tmp) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 13%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder__tmp) <= 0 /\ 1 * (s IDadpcm_coder_bufferstep) <= 0 /\ -1 * (s IDadpcm_coder_bufferstep) <= 0)%Z
    | 14%positive => (-1 * (s IDadpcm_coder_bufferstep) <= 0 /\ 1 * (s IDadpcm_coder_bufferstep) <= 0 /\ 1 * (s IDadpcm_coder__tmp) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 15%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder__tmp) <= 0)%Z
    | 16%positive => (1 * (s IDadpcm_coder__tmp) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 17%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder__tmp) <= 0)%Z
    | 18%positive => (1 * (s IDadpcm_coder__tmp) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 19%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 22%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 24%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 26%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_coder_sign) <= 0 /\ -1 * (s IDadpcm_coder_sign) <= 0)%Z
    | 27%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 29%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 31%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0)%Z
    | 32%positive => (-1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 33%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0)%Z
    | 34%positive => (-1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_diff)+ -1 * (s IDadpcm_coder_step) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0)%Z
    | 36%positive => (-1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0)%Z
    | 37%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_delta) + 4 <= 0)%Z
    | 38%positive => (-1 * (s IDadpcm_coder_delta) + 4 <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_diff) <= 0)%Z
    | 39%positive => (-1 * (s IDadpcm_coder_diff) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_delta) + 4 <= 0)%Z
    | 40%positive => (-1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0)%Z
    | 42%positive => (-1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_diff)+ -1 * (s IDadpcm_coder_step) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0)%Z
    | 45%positive => (-1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0 /\ -1 * (s IDadpcm_coder_delta) <= 0 /\ 1 * (s IDadpcm_coder_delta) + -4 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0)%Z
    | 47%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_diff) <= 0)%Z
    | 48%positive => (-1 * (s IDadpcm_coder_diff) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 49%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 51%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_diff)+ -1 * (s IDadpcm_coder_step) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0)%Z
    | 54%positive => (-1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0)%Z
    | 56%positive => (-1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_diff)+ 1 * (s IDadpcm_coder_step) <= 0)%Z
    | 58%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_sign) <= 0 /\ -1 * (s IDadpcm_coder_sign) <= 0)%Z
    | 60%positive => (-1 * (s IDadpcm_coder_sign) <= 0 /\ 1 * (s IDadpcm_coder_sign) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_sign) <= 0 /\ -1 * (s IDadpcm_coder_sign) <= 0)%Z
    | 62%positive => (-1 * (s IDadpcm_coder_sign) <= 0 /\ 1 * (s IDadpcm_coder_sign) <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 64%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 66%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 68%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 69%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 70%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 72%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 75%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 77%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 78%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 79%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 80%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_coder_index) + 1 <= 0)%Z
    | 81%positive => (1 * (s IDadpcm_coder_index) + 1 <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 82%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_coder_index) <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 83%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 84%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 85%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0)%Z
    | 86%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder_index) + 89 <= 0)%Z
    | 87%positive => (-1 * (s IDadpcm_coder_index) + 89 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 88%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) + 88 <= 0)%Z
    | 89%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 90%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 91%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 92%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_bufferstep) <= 0 /\ -1 * (s IDadpcm_coder_bufferstep) <= 0)%Z
    | 93%positive => (-1 * (s IDadpcm_coder_bufferstep) <= 0 /\ 1 * (s IDadpcm_coder_bufferstep) <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 94%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 95%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 96%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 97%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 98%positive => (-1 * (s IDadpcm_coder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0)%Z
    | 99%positive => (-1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0 /\ -1 * (s IDadpcm_coder__tmp) + 1 <= 0)%Z
    | 100%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0 /\ -1 * (s IDadpcm_coder__tmp) <= 0)%Z
    | 101%positive => (-1 * (s IDadpcm_coder__tmp) <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) <= 0)%Z
    | 102%positive => (-1 * (s IDadpcm_coder_z) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0 /\ -1 * (s IDadpcm_coder__tmp) <= 0)%Z
    | 103%positive => (-1 * (s IDadpcm_coder__tmp) <= 0 /\ -1 * (s IDadpcm_coder_index) <= 0 /\ 1 * (s IDadpcm_coder_index) + -88 <= 0 /\ -1 * (s IDadpcm_coder_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition adpcm_coder_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDadpcm_coder_len)))%Q
    | 2%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder_len)))%Q
    | 3%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 4%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 5%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 6%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 7%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 8%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 9%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 10%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 11%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 12%positive => ((s IDadpcm_coder_z) - max0(-1 + (s IDadpcm_coder__tmp))
                      + max0((s IDadpcm_coder__tmp)))%Q
    | 13%positive => ((s IDadpcm_coder_z) - max0(-1 + (s IDadpcm_coder__tmp))
                      + max0((s IDadpcm_coder__tmp)))%Q
    | 14%positive => ((s IDadpcm_coder_z))%Q
    | 15%positive => ((s IDadpcm_coder_z) - max0(-1 + (s IDadpcm_coder__tmp))
                      + max0((s IDadpcm_coder__tmp)))%Q
    | 16%positive => ((s IDadpcm_coder_z))%Q
    | 17%positive => ((s IDadpcm_coder_z))%Q
    | 18%positive => ((s IDadpcm_coder_z))%Q
    | 19%positive => ((s IDadpcm_coder_z))%Q
    | 20%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | 21%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 22%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 23%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 24%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 25%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 26%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 27%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 28%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 29%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 30%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 31%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 32%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 33%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 34%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 35%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 36%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 37%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 38%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 39%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 40%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 41%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 42%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 43%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 44%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 45%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 46%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 47%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 48%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 49%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 50%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 51%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 52%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 53%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 54%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 55%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 56%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 57%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 58%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 59%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 60%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 61%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 62%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 63%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 64%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 65%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 66%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 67%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 68%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 69%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 70%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 71%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 72%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 73%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 74%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 75%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 76%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 77%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 78%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 79%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 80%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 81%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 82%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 83%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 84%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 85%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 86%positive => ((s IDadpcm_coder__tmp) + (s IDadpcm_coder_z))%Q
    | 87%positive => ((1 # 1) + (s IDadpcm_coder__tmp) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp))
                      - max0((s IDadpcm_coder__tmp)))%Q
    | 88%positive => ((1 # 1) + (s IDadpcm_coder__tmp) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp))
                      - max0((s IDadpcm_coder__tmp)))%Q
    | 89%positive => ((1 # 1) + (s IDadpcm_coder__tmp) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp))
                      - max0((s IDadpcm_coder__tmp)))%Q
    | 90%positive => ((1 # 1) + (s IDadpcm_coder__tmp) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp))
                      - max0((s IDadpcm_coder__tmp)))%Q
    | 91%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 92%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 93%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 94%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 95%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 96%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 97%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 98%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 99%positive => ((1 # 1) + (s IDadpcm_coder_z)
                      + max0(-1 + (s IDadpcm_coder__tmp)))%Q
    | 100%positive => ((1 # 1) + (s IDadpcm_coder_z)
                       + max0((s IDadpcm_coder__tmp)))%Q
    | 101%positive => ((1 # 1) + (s IDadpcm_coder_z)
                       + max0((s IDadpcm_coder__tmp)))%Q
    | 102%positive => ((1 # 1) + (s IDadpcm_coder_z)
                       + max0((s IDadpcm_coder__tmp)))%Q
    | 103%positive => ((s IDadpcm_coder_z) + max0((s IDadpcm_coder__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition adpcm_coder_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDadpcm_coder__tmp))]
    | 12%positive => []
    | 13%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDadpcm_coder__tmp)) (-1
                                                                    + (s IDadpcm_coder__tmp)))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDadpcm_coder__tmp)) (-1
                                                                    + (s IDadpcm_coder__tmp)))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadpcm_coder__tmp))) (F_check_ge ((s IDadpcm_coder__tmp)) (0))]
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
    | 85%positive => [(*-1 0*) F_max0_pre_decrement ((s IDadpcm_coder__tmp)) (1)]
    | 86%positive => [(*-1 0*) F_max0_pre_decrement ((s IDadpcm_coder__tmp)) (1)]
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDadpcm_coder__tmp)) (0))) (F_max0_ge_0 ((s IDadpcm_coder__tmp)))]
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
    | _ => []
  end.


Theorem adpcm_coder_ai_correct:
  forall s p' s', steps (g_start adpcm_coder) s (g_edges adpcm_coder) p' s' -> adpcm_coder_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem adpcm_coder_pot_correct:
  forall s p' s',
    steps (g_start adpcm_coder) s (g_edges adpcm_coder) p' s' ->
    (adpcm_coder_pot (g_start adpcm_coder) s >= adpcm_coder_pot p' s')%Q.
Proof.
  check_lp adpcm_coder_ai_correct adpcm_coder_hints.
Qed.

