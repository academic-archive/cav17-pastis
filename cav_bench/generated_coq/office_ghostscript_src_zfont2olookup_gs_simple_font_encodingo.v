Require Import pasta.Pasta.

Notation IDlookup_gs_simple_font_encoding_z := 1%positive.
Notation IDlookup_gs_simple_font_encoding_best := 2%positive.
Notation IDlookup_gs_simple_font_encoding_esize := 3%positive.
Notation IDlookup_gs_simple_font_encoding_f_packed := 4%positive.
Notation IDlookup_gs_simple_font_encoding_i := 5%positive.
Notation IDlookup_gs_simple_font_encoding_index := 6%positive.
Notation IDlookup_gs_simple_font_encoding_match := 7%positive.
Notation IDlookup_gs_simple_font_encoding_near_index := 8%positive.
Notation IDlookup_gs_simple_font_encoding_pfont_dref_off328 := 9%positive.
Notation IDlookup_gs_simple_font_encoding_pfont_dref_off332 := 10%positive.
Notation IDlookup_gs_simple_font_encoding_r_packed := 11%positive.
Notation IDlookup_gs_simple_font_encoding_rnidx := 12%positive.
Notation IDlookup_gs_simple_font_encoding_pfont := 13%positive.
Definition lookup_gs_simple_font_encoding : graph := {|
  g_start := 1%positive;
  g_end := 35%positive;
  g_edges := (1%positive,(AAssign IDlookup_gs_simple_font_encoding_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_match)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_best)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDlookup_gs_simple_font_encoding_index
             (Some (ENum (5)))),6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDlookup_gs_simple_font_encoding_index
             (Some (EAdd (EVar IDlookup_gs_simple_font_encoding_index)
             (ENum (-1))))),8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_index)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_index)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,17%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,16%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDlookup_gs_simple_font_encoding_z
             (Some (EAdd (ENum (1))
             (EVar IDlookup_gs_simple_font_encoding_z)))),7%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign
             IDlookup_gs_simple_font_encoding_pfont_dref_off328
             (Some (EVar IDlookup_gs_simple_font_encoding_index))),
             18%positive)::(18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_index)
             s) < (eval (ENum (0)) s))%Z)),21%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_index)
             s) >= (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,33%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign
             IDlookup_gs_simple_font_encoding_near_index (Some (ENum (-1)))),
             23%positive)::
             (23%positive,(AAssign IDlookup_gs_simple_font_encoding_esize
             None),24%positive)::
             (24%positive,(AAssign IDlookup_gs_simple_font_encoding_best
             None),25%positive)::
             (25%positive,(AAssign IDlookup_gs_simple_font_encoding_index
             (Some (ENum (5)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDlookup_gs_simple_font_encoding_index
             (Some (EAdd (EVar IDlookup_gs_simple_font_encoding_index)
             (ENum (-1))))),28%positive)::(28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_index)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),36%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_index)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AAssign IDlookup_gs_simple_font_encoding_index
             (Some (EVar IDlookup_gs_simple_font_encoding_near_index))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,(AAssign
             IDlookup_gs_simple_font_encoding_pfont_dref_off332
             (Some (EVar IDlookup_gs_simple_font_encoding_index))),
             34%positive)::(34%positive,AWeaken,35%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDlookup_gs_simple_font_encoding_r_packed
             None),38%positive)::
             (38%positive,(AAssign IDlookup_gs_simple_font_encoding_f_packed
             None),39%positive)::
             (39%positive,(AAssign IDlookup_gs_simple_font_encoding_match
             (Some (EVar IDlookup_gs_simple_font_encoding_esize))),
             40%positive)::(40%positive,AWeaken,41%positive)::
             (41%positive,ANone,86%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDlookup_gs_simple_font_encoding_i
             (Some (EVar IDlookup_gs_simple_font_encoding_esize))),
             43%positive)::(43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDlookup_gs_simple_font_encoding_i
             (Some (EAdd (EVar IDlookup_gs_simple_font_encoding_i)
             (ENum (-1))))),45%positive)::(45%positive,AWeaken,46%positive)::
             (46%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_i)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),48%positive)::
             (46%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_i)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),47%positive)::
             (47%positive,AWeaken,79%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_r_packed)
             s) <> (eval (ENum (0)) s))%Z)),54%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_r_packed)
             s) = (eval (ENum (0)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AAssign IDlookup_gs_simple_font_encoding_rnidx
             None),52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,AWeaken,58%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AAssign IDlookup_gs_simple_font_encoding_rnidx
             None),56%positive)::(56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_f_packed)
             s) <> (eval (ENum (0)) s))%Z)),62%positive)::
             (58%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_f_packed)
             s) = (eval (ENum (0)) s))%Z)),59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,65%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,66%positive)::
             (65%positive,ANone,68%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,68%positive)::
             (67%positive,ANone,73%positive)::
             (68%positive,(AAssign IDlookup_gs_simple_font_encoding_match
             (Some (EAdd (EVar IDlookup_gs_simple_font_encoding_match)
             (ENum (-1))))),69%positive)::(69%positive,AWeaken,70%positive)::
             (70%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_match)
             (ENum (-1))) s) <=
             (eval (EVar IDlookup_gs_simple_font_encoding_best) s))%Z)),
             76%positive)::
             (70%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDlookup_gs_simple_font_encoding_match)
             (ENum (-1))) s) >
             (eval (EVar IDlookup_gs_simple_font_encoding_best) s))%Z)),
             71%positive)::(71%positive,AWeaken,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDlookup_gs_simple_font_encoding_z
             (Some (EAdd (ENum (1))
             (EVar IDlookup_gs_simple_font_encoding_z)))),44%positive)::
             (76%positive,AWeaken,77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_match)
             s) > (eval (EVar IDlookup_gs_simple_font_encoding_best) s))%Z)),
             81%positive)::
             (79%positive,(AGuard
             (fun s => ((eval (EVar IDlookup_gs_simple_font_encoding_match)
             s) <= (eval (EVar IDlookup_gs_simple_font_encoding_best)
             s))%Z)),80%positive)::(80%positive,AWeaken,85%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,(AAssign IDlookup_gs_simple_font_encoding_best
             (Some (EVar IDlookup_gs_simple_font_encoding_match))),
             83%positive)::
             (83%positive,(AAssign
             IDlookup_gs_simple_font_encoding_near_index
             (Some (EVar IDlookup_gs_simple_font_encoding_index))),
             84%positive)::(84%positive,ANone,85%positive)::
             (85%positive,ANone,87%positive)::
             (86%positive,ANone,87%positive)::
             (87%positive,ANone,88%positive)::
             (88%positive,(AAssign IDlookup_gs_simple_font_encoding_z
             (Some (EAdd (ENum (1))
             (EVar IDlookup_gs_simple_font_encoding_z)))),27%positive)::nil
|}.

Definition lookup_gs_simple_font_encoding_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0)%Z
    | 3%positive => (-1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 4%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0)%Z
    | 5%positive => (-1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 6%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -5 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) + 5 <= 0)%Z
    | 7%positive => (1 * (s IDlookup_gs_simple_font_encoding_index) + -5 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 8%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0)%Z
    | 9%positive => (-1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 10%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) <= 0)%Z
    | 11%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 13%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 15%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 18%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) <= 0)%Z
    | 19%positive => (-1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 20%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) <= 0)%Z
    | 21%positive => (False)%Z
    | 22%positive => (False)%Z
    | 23%positive => (False)%Z
    | 24%positive => (False)%Z
    | 25%positive => (False)%Z
    | 26%positive => (False)%Z
    | 27%positive => (False)%Z
    | 28%positive => (False)%Z
    | 29%positive => (False)%Z
    | 30%positive => (False)%Z
    | 31%positive => (False)%Z
    | 32%positive => (False)%Z
    | 33%positive => (-1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 34%positive => (-1 * (s IDlookup_gs_simple_font_encoding_match) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off332) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off332) <= 0)%Z
    | 35%positive => (-1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off332) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off332) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_pfont_dref_off328) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_index) <= 0 /\ 1 * (s IDlookup_gs_simple_font_encoding_index) + -4 <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_z) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_best) <= 0 /\ -1 * (s IDlookup_gs_simple_font_encoding_match) <= 0)%Z
    | 36%positive => (False)%Z
    | 37%positive => (False)%Z
    | 38%positive => (False)%Z
    | 39%positive => (False)%Z
    | 40%positive => (False)%Z
    | 41%positive => (False)%Z
    | 42%positive => (False)%Z
    | 43%positive => (False)%Z
    | 44%positive => (False)%Z
    | 45%positive => (False)%Z
    | 46%positive => (False)%Z
    | 47%positive => (False)%Z
    | 48%positive => (False)%Z
    | 49%positive => (False)%Z
    | 50%positive => (False)%Z
    | 51%positive => (False)%Z
    | 52%positive => (False)%Z
    | 53%positive => (False)%Z
    | 54%positive => (False)%Z
    | 55%positive => (False)%Z
    | 56%positive => (False)%Z
    | 57%positive => (False)%Z
    | 58%positive => (False)%Z
    | 59%positive => (False)%Z
    | 60%positive => (False)%Z
    | 61%positive => (False)%Z
    | 62%positive => (False)%Z
    | 63%positive => (False)%Z
    | 64%positive => (False)%Z
    | 65%positive => (False)%Z
    | 66%positive => (False)%Z
    | 67%positive => (False)%Z
    | 68%positive => (False)%Z
    | 69%positive => (False)%Z
    | 70%positive => (False)%Z
    | 71%positive => (False)%Z
    | 72%positive => (False)%Z
    | 73%positive => (False)%Z
    | 74%positive => (False)%Z
    | 75%positive => (False)%Z
    | 76%positive => (False)%Z
    | 77%positive => (False)%Z
    | 78%positive => (False)%Z
    | 79%positive => (False)%Z
    | 80%positive => (False)%Z
    | 81%positive => (False)%Z
    | 82%positive => (False)%Z
    | 83%positive => (False)%Z
    | 84%positive => (False)%Z
    | 85%positive => (False)%Z
    | 86%positive => (False)%Z
    | 87%positive => (False)%Z
    | 88%positive => (False)%Z
    | _ => False
  end.

Definition lookup_gs_simple_font_encoding_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDlookup_gs_simple_font_encoding_z))%Q
    | 3%positive => ((4 # 1) + (s IDlookup_gs_simple_font_encoding_z))%Q
    | 4%positive => ((4 # 1) + (s IDlookup_gs_simple_font_encoding_z))%Q
    | 5%positive => ((4 # 1) + (s IDlookup_gs_simple_font_encoding_z))%Q
    | 6%positive => ((3 # 2)
                     + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                     + (s IDlookup_gs_simple_font_encoding_z)
                     - (1 # 2) * max0(5
                                      - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 7%positive => ((3 # 2)
                     + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                     + (s IDlookup_gs_simple_font_encoding_z)
                     - (1 # 2) * max0(5
                                      - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 8%positive => ((2 # 1)
                     + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                     + (s IDlookup_gs_simple_font_encoding_z)
                     - (1 # 2) * max0(4
                                      - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 9%positive => ((9 # 2) + (s IDlookup_gs_simple_font_encoding_z)
                     - (1 # 2) * max0(4
                                      - (s IDlookup_gs_simple_font_encoding_index))
                     - (1 # 2) * max0(5
                                      - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 10%positive => ((9 # 2) + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(4
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 11%positive => ((9 # 2) + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(4
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 12%positive => ((5 # 2)
                      + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 13%positive => ((5 # 2)
                      + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 14%positive => ((5 # 2)
                      + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 15%positive => ((5 # 2)
                      + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 16%positive => ((5 # 2)
                      + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 17%positive => ((5 # 2)
                      + (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 18%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 19%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 20%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 21%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 22%positive => ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z))%Q
    | 23%positive => (-(1 # 6)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 24%positive => (-(1 # 6)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 25%positive => (-(1 # 6)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 26%positive => (-(1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 27%positive => (-(1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 28%positive => (-(5 # 6)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 29%positive => ((2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 30%positive => ((2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 31%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 32%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 33%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 34%positive => ((5 # 2)
                      - (1 # 2) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 2) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 35%positive => ((s IDlookup_gs_simple_font_encoding_z))%Q
    | 36%positive => ((2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 37%positive => ((5 # 6)
                      - (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 6) * max0(-5
                                       + (s IDlookup_gs_simple_font_encoding_index))
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 38%positive => ((5 # 6)
                      - (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 6) * max0(-5
                                       + (s IDlookup_gs_simple_font_encoding_index))
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 39%positive => ((5 # 6)
                      - (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 6) * max0(-5
                                       + (s IDlookup_gs_simple_font_encoding_index))
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 40%positive => ((5 # 6)
                      - (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 6) * max0(-5
                                       + (s IDlookup_gs_simple_font_encoding_index))
                      - (1 # 6) * max0(5
                                       - (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))%Q
    | 41%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 42%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 43%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 44%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 45%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 46%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 47%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 48%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 49%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 50%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 51%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 52%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 53%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 54%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 55%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 56%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 57%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 20) * max0(-5
                                         + (s IDlookup_gs_simple_font_encoding_near_index))
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 58%positive => (-(1 # 4)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (37 # 83) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 59%positive => (-(1 # 4)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (37 # 83) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 60%positive => (-(1 # 4)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (120 # 83) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0(-(s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 61%positive => (-(1 # 4)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (120 # 83) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index))
                      + max0(-(s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 62%positive => (-(1 # 4)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (37 # 83) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 63%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 64%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (19 # 48) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (21 # 16) * max0(4
                                         - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 65%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 66%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 67%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 68%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 69%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 70%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 71%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 72%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 73%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 74%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 75%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 76%positive => ((1 # 1)
                      + (1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 77%positive => ((1 # 1)
                      - (1 # 30) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 5) * max0(-1
                                       + (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 78%positive => ((1 # 1)
                      - (1 # 30) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 5) * max0(-1
                                       + (s IDlookup_gs_simple_font_encoding_index))
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 79%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 80%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 81%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 82%positive => ((3 # 10) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 83%positive => ((3 # 10) * (s IDlookup_gs_simple_font_encoding_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_index)))%Q
    | 84%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 85%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 86%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 87%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | 88%positive => ((1 # 6) * (s IDlookup_gs_simple_font_encoding_index)
                      + (2 # 15) * (s IDlookup_gs_simple_font_encoding_near_index)
                      + (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)
                      + (s IDlookup_gs_simple_font_encoding_z)
                      + (1 # 20) * max0(5
                                        - (s IDlookup_gs_simple_font_encoding_near_index)))%Q
    | _ => (0 # 1)%Q
  end.

Definition lookup_gs_simple_font_encoding_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                                   - 
                                                                   (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))]
    | 9%positive => []
    | 10%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDlookup_gs_simple_font_encoding_index)))]
    | 11%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDlookup_gs_simple_font_encoding_index)))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)))]
    | 21%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)));
                      (*0 0.166667*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))]
    | 29%positive => []
    | 30%positive => [(*-0.166667 0*) F_max0_pre_decrement ((s IDlookup_gs_simple_font_encoding_index)) (1);
                      (*-0.166667 0*) F_max0_ge_0 (-1
                                                   + (s IDlookup_gs_simple_font_encoding_index));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge (0) (0));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_index)));
                      (*-0.55 0*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_near_index)))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)))]
    | 35%positive => []
    | 36%positive => [(*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_index)))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (0) (0));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (-
                                                                    (s IDlookup_gs_simple_font_encoding_index)));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (0) (0))]
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*0 0.05*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                   - 
                                                                   (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0));
                      (*-8.33555e-13 1.3125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)));
                      (*1.36642e-12 1.05*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_near_index)))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)));
                      (*-1.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                                    + 
                                                                    (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0))]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_pfont_dref_off328)));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)));
                      (*-1.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                                    + 
                                                                    (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0))]
    | 58%positive => []
    | 59%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (-
                                                                    (s IDlookup_gs_simple_font_encoding_near_index)))]
    | 60%positive => []
    | 61%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                 - (s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)));
                      (*-1.3125 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))]
    | 62%positive => [(*0 0.05*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                                    - 
                                                                    (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))]
    | 63%positive => []
    | 64%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (-
                                                                    (s IDlookup_gs_simple_font_encoding_near_index)));
                      (*0 0.05*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                 - (s IDlookup_gs_simple_font_encoding_pfont_dref_off328))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDlookup_gs_simple_font_encoding_pfont_dref_off328)));
                      (*-1.3125 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (4
                                                                    - (s IDlookup_gs_simple_font_encoding_near_index)) (0))]
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
    | 76%positive => [(*0 0.2*) F_max0_monotonic (F_check_ge ((s IDlookup_gs_simple_font_encoding_index)) (-1
                                                                    + (s IDlookup_gs_simple_font_encoding_index)));
                      (*0 0.2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_index)))]
    | 77%positive => []
    | 78%positive => [(*-0.2 0*) F_max0_ge_0 (-1
                                              + (s IDlookup_gs_simple_font_encoding_index));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                   - 
                                                                   (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (0) (0));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)))]
    | 79%positive => []
    | 80%positive => []
    | 81%positive => [(*-0.133333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0));
                      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDlookup_gs_simple_font_encoding_near_index)) (0))) (F_max0_ge_0 ((s IDlookup_gs_simple_font_encoding_near_index)));
                      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (0) (0));
                      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (-
                                                                    (s IDlookup_gs_simple_font_encoding_index)));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                                    - 
                                                                    (s IDlookup_gs_simple_font_encoding_near_index))) (F_check_ge (0) (0));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (5
                                                                    - (s IDlookup_gs_simple_font_encoding_index)));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                                    + 
                                                                    (s IDlookup_gs_simple_font_encoding_index))) (F_check_ge (0) (0));
                      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_index)) (0))) (F_max0_ge_0 (-5
                                                                    + (s IDlookup_gs_simple_font_encoding_index)))]
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | _ => []
  end.


Theorem lookup_gs_simple_font_encoding_ai_correct:
  forall s p' s', steps (g_start lookup_gs_simple_font_encoding) s (g_edges lookup_gs_simple_font_encoding) p' s' -> lookup_gs_simple_font_encoding_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem lookup_gs_simple_font_encoding_pot_correct:
  forall s p' s',
    steps (g_start lookup_gs_simple_font_encoding) s (g_edges lookup_gs_simple_font_encoding) p' s' ->
    (lookup_gs_simple_font_encoding_pot (g_start lookup_gs_simple_font_encoding) s >= lookup_gs_simple_font_encoding_pot p' s')%Q.
Proof.
  check_lp lookup_gs_simple_font_encoding_ai_correct lookup_gs_simple_font_encoding_hints.
Qed.

