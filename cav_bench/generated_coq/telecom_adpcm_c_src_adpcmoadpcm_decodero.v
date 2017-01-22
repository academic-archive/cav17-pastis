Require Import pasta.Pasta.

Notation IDadpcm_decoder_z := 1%positive.
Notation IDadpcm_decoder__tmp := 2%positive.
Notation IDadpcm_decoder_bufferstep := 3%positive.
Notation IDadpcm_decoder_delta := 4%positive.
Notation IDadpcm_decoder_index := 5%positive.
Notation IDadpcm_decoder_inputbuffer := 6%positive.
Notation IDadpcm_decoder_sign := 7%positive.
Notation IDadpcm_decoder_state_dref_off0 := 8%positive.
Notation IDadpcm_decoder_state_dref_off2 := 9%positive.
Notation IDadpcm_decoder_step := 10%positive.
Notation IDadpcm_decoder_valpred := 11%positive.
Notation IDadpcm_decoder_vpdiff := 12%positive.
Notation IDadpcm_decoder_indata := 13%positive.
Notation IDadpcm_decoder_len := 14%positive.
Notation IDadpcm_decoder_outdata := 15%positive.
Notation IDadpcm_decoder_state := 16%positive.
Definition adpcm_decoder : graph := {|
  g_start := 1%positive;
  g_end := 15%positive;
  g_edges := (1%positive,(AAssign IDadpcm_decoder_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDadpcm_decoder__tmp
             (Some (EVar IDadpcm_decoder_len))),3%positive)::
             (3%positive,(AAssign IDadpcm_decoder_inputbuffer
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDadpcm_decoder_valpred
             (Some (EVar IDadpcm_decoder_state_dref_off0))),5%positive)::
             (5%positive,(AAssign IDadpcm_decoder_index
             (Some (EVar IDadpcm_decoder_state_dref_off2))),6%positive)::
             (6%positive,(AAssign IDadpcm_decoder_step None),7%positive)::
             (7%positive,(AAssign IDadpcm_decoder_bufferstep
             (Some (ENum (0)))),8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder__tmp) s) >
             (eval (ENum (0)) s))%Z)),16%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder__tmp) s) <=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDadpcm_decoder_state_dref_off0
             (Some (EVar IDadpcm_decoder_valpred))),13%positive)::
             (13%positive,(AAssign IDadpcm_decoder_state_dref_off2
             (Some (EVar IDadpcm_decoder_index))),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_bufferstep) s) <>
             (eval (ENum (0)) s))%Z)),22%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_bufferstep) s) =
             (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDadpcm_decoder_inputbuffer None),
             20%positive)::
             (20%positive,(AAssign IDadpcm_decoder_delta None),21%positive)::
             (21%positive,ANone,25%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AAssign IDadpcm_decoder_delta None),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDadpcm_decoder_bufferstep None),
             26%positive)::
             (26%positive,(AAssign IDadpcm_decoder_index None),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_index) s) <
             (eval (ENum (0)) s))%Z)),30%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_index) s) >=
             (eval (ENum (0)) s))%Z)),29%positive)::
             (29%positive,AWeaken,34%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AAssign IDadpcm_decoder_index (Some (ENum (0)))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_index) s) >
             (eval (ENum (88)) s))%Z)),36%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_index) s) <=
             (eval (ENum (88)) s))%Z)),35%positive)::
             (35%positive,AWeaken,39%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDadpcm_decoder_index (Some (ENum (88)))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDadpcm_decoder_sign None),40%positive)::
             (40%positive,(AAssign IDadpcm_decoder_delta None),41%positive)::
             (41%positive,(AAssign IDadpcm_decoder_vpdiff None),42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,45%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,48%positive)::
             (45%positive,(AAssign IDadpcm_decoder_vpdiff
             (Some (EAdd (EVar IDadpcm_decoder_vpdiff)
             (EVar IDadpcm_decoder_step)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,50%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,53%positive)::
             (50%positive,(AAssign IDadpcm_decoder_vpdiff None),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,55%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,58%positive)::
             (55%positive,(AAssign IDadpcm_decoder_vpdiff None),56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_sign) s) <>
             (eval (ENum (0)) s))%Z)),63%positive)::
             (58%positive,(AGuard
             (fun s => ((eval (EVar IDadpcm_decoder_sign) s) =
             (eval (ENum (0)) s))%Z)),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AAssign IDadpcm_decoder_valpred
             (Some (EAdd (EVar IDadpcm_decoder_valpred)
             (EVar IDadpcm_decoder_vpdiff)))),61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,AWeaken,67%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDadpcm_decoder_valpred
             (Some (ESub (EVar IDadpcm_decoder_valpred)
             (EVar IDadpcm_decoder_vpdiff)))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,73%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,70%positive)::
             (69%positive,ANone,72%positive)::
             (70%positive,(AAssign IDadpcm_decoder_valpred None),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,75%positive)::
             (73%positive,(AAssign IDadpcm_decoder_valpred None),74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDadpcm_decoder_step None),76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,(AAssign IDadpcm_decoder__tmp
             (Some (EAdd (EVar IDadpcm_decoder__tmp) (ENum (-1))))),
             78%positive)::(78%positive,ANone,79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,(AAssign IDadpcm_decoder_z (Some (EAdd (ENum (1))
             (EVar IDadpcm_decoder_z)))),81%positive)::
             (81%positive,AWeaken,10%positive)::nil
|}.

Definition adpcm_decoder_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 3%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 4%positive => (1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ -1 * (s IDadpcm_decoder_inputbuffer) <= 0)%Z
    | 5%positive => (-1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ 1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 6%positive => (1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ -1 * (s IDadpcm_decoder_inputbuffer) <= 0)%Z
    | 7%positive => (-1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ 1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 8%positive => (1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ -1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ 1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ -1 * (s IDadpcm_decoder_bufferstep) <= 0)%Z
    | 9%positive => (-1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ 1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ -1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ 1 * (s IDadpcm_decoder_inputbuffer) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 10%positive => (-1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 11%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder__tmp) <= 0)%Z
    | 12%positive => (1 * (s IDadpcm_decoder__tmp) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 13%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder__tmp) <= 0)%Z
    | 14%positive => (1 * (s IDadpcm_decoder__tmp) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 15%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 18%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ -1 * (s IDadpcm_decoder_bufferstep) <= 0)%Z
    | 19%positive => (-1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ 1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 20%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ -1 * (s IDadpcm_decoder_bufferstep) <= 0)%Z
    | 21%positive => (-1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ 1 * (s IDadpcm_decoder_bufferstep) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 22%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 24%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 26%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 28%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 30%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_index) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDadpcm_decoder_index) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_index) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 33%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 35%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0)%Z
    | 36%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_index) + 89 <= 0)%Z
    | 37%positive => (-1 * (s IDadpcm_decoder_index) + 89 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 38%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) + 88 <= 0)%Z
    | 39%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 40%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 41%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 42%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 43%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 44%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 45%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 46%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 47%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 48%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 49%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 50%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 51%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 52%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 53%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 54%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 55%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 56%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 57%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 58%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 59%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_sign) <= 0 /\ -1 * (s IDadpcm_decoder_sign) <= 0)%Z
    | 60%positive => (-1 * (s IDadpcm_decoder_sign) <= 0 /\ 1 * (s IDadpcm_decoder_sign) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 61%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_sign) <= 0 /\ -1 * (s IDadpcm_decoder_sign) <= 0)%Z
    | 62%positive => (-1 * (s IDadpcm_decoder_sign) <= 0 /\ 1 * (s IDadpcm_decoder_sign) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 63%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 64%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 65%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 66%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 67%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 68%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 69%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 70%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 71%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 72%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 73%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 74%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 75%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 76%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0)%Z
    | 77%positive => (-1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder__tmp) + 1 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 78%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) <= 0)%Z
    | 79%positive => (-1 * (s IDadpcm_decoder__tmp) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_z) <= 0)%Z
    | 80%positive => (-1 * (s IDadpcm_decoder_z) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ -1 * (s IDadpcm_decoder__tmp) <= 0)%Z
    | 81%positive => (-1 * (s IDadpcm_decoder__tmp) <= 0 /\ -1 * (s IDadpcm_decoder_index) <= 0 /\ 1 * (s IDadpcm_decoder_index) + -88 <= 0 /\ -1 * (s IDadpcm_decoder_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition adpcm_decoder_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDadpcm_decoder_len)))%Q
    | 2%positive => (max0((s IDadpcm_decoder_len))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 3%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 4%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 5%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 6%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 7%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 8%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 9%positive => (max0((s IDadpcm_decoder__tmp))
                     + max0((s IDadpcm_decoder_z)))%Q
    | 10%positive => (max0((s IDadpcm_decoder__tmp))
                      + max0((s IDadpcm_decoder_z)))%Q
    | 11%positive => (max0((s IDadpcm_decoder__tmp))
                      + max0((s IDadpcm_decoder_z)))%Q
    | 12%positive => (max0((s IDadpcm_decoder__tmp))
                      + max0((s IDadpcm_decoder_z)))%Q
    | 13%positive => (max0((s IDadpcm_decoder__tmp))
                      + max0((s IDadpcm_decoder_z)))%Q
    | 14%positive => (max0((s IDadpcm_decoder__tmp))
                      + max0((s IDadpcm_decoder_z)))%Q
    | 15%positive => ((s IDadpcm_decoder_z))%Q
    | 16%positive => (max0((s IDadpcm_decoder__tmp))
                      + max0((s IDadpcm_decoder_z)))%Q
    | 17%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 18%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 19%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 20%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 21%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 22%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 23%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 24%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 25%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 26%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 27%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 28%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 29%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 30%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 31%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 32%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 33%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 34%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 35%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 36%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 37%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 38%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 39%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 40%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 41%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 42%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 43%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 44%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 45%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 46%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 47%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 48%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 49%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 50%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 51%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 52%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 53%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 54%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 55%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 56%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 57%positive => ((s IDadpcm_decoder__tmp) + max0((s IDadpcm_decoder_z)))%Q
    | 58%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 59%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 60%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 61%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 62%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 63%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 64%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 65%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 66%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 67%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 68%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 69%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 70%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 71%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 72%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 73%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 74%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 75%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 76%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 77%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | 78%positive => ((1 # 1) + (s IDadpcm_decoder__tmp)
                      + (s IDadpcm_decoder_z))%Q
    | 79%positive => ((1 # 1) + (s IDadpcm_decoder__tmp)
                      + (s IDadpcm_decoder_z))%Q
    | 80%positive => ((1 # 1) + (s IDadpcm_decoder__tmp)
                      + (s IDadpcm_decoder_z))%Q
    | 81%positive => ((s IDadpcm_decoder__tmp) + (s IDadpcm_decoder_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition adpcm_decoder_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDadpcm_decoder__tmp)) (-1
                                                                    + (s IDadpcm_decoder__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDadpcm_decoder__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadpcm_decoder_z))) (F_check_ge ((s IDadpcm_decoder_z)) (0))]
    | 15%positive => []
    | 16%positive => [(*0 1*) F_max0_pre_decrement ((s IDadpcm_decoder__tmp)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDadpcm_decoder__tmp))) (F_check_ge (-1
                                                                    + (s IDadpcm_decoder__tmp)) (0))]
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
    | 35%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadpcm_decoder_z))) (F_check_ge ((s IDadpcm_decoder_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDadpcm_decoder_z)) (0))) (F_max0_ge_0 ((s IDadpcm_decoder_z)))]
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
    | 54%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadpcm_decoder_z))) (F_check_ge ((s IDadpcm_decoder_z)) (0))]
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadpcm_decoder_z))) (F_check_ge ((s IDadpcm_decoder_z)) (0))]
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
    | 81%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDadpcm_decoder_z)) (0))) (F_max0_ge_0 ((s IDadpcm_decoder_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDadpcm_decoder__tmp)) (0))) (F_max0_ge_0 ((s IDadpcm_decoder__tmp)))]
    | _ => []
  end.


Theorem adpcm_decoder_ai_correct:
  forall s p' s', steps (g_start adpcm_decoder) s (g_edges adpcm_decoder) p' s' -> adpcm_decoder_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem adpcm_decoder_pot_correct:
  forall s p' s',
    steps (g_start adpcm_decoder) s (g_edges adpcm_decoder) p' s' ->
    (adpcm_decoder_pot (g_start adpcm_decoder) s >= adpcm_decoder_pot p' s')%Q.
Proof.
  check_lp adpcm_decoder_ai_correct adpcm_decoder_hints.
Qed.

