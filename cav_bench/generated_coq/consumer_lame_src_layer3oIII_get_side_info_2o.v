Require Import pasta.Pasta.

Notation IDIII_get_side_info_2_z := 1%positive.
Notation IDIII_get_side_info_2__tmp := 2%positive.
Notation IDIII_get_side_info_2__tmp1 := 3%positive.
Notation IDIII_get_side_info_2__tmp2 := 4%positive.
Notation IDIII_get_side_info_2__tmp3 := 5%positive.
Notation IDIII_get_side_info_2_ch := 6%positive.
Notation IDIII_get_side_info_2_i := 7%positive.
Notation IDIII_get_side_info_2_i1 := 8%positive.
Notation IDIII_get_side_info_2_powdiff := 9%positive.
Notation IDIII_get_side_info_2_qss := 10%positive.
Notation IDIII_get_side_info_2_r0c := 11%positive.
Notation IDIII_get_side_info_2_r1c := 12%positive.
Notation IDIII_get_side_info_2_sbg := 13%positive.
Notation IDIII_get_side_info_2_ms_stereo := 14%positive.
Notation IDIII_get_side_info_2_sfreq := 15%positive.
Notation IDIII_get_side_info_2_si := 16%positive.
Notation IDIII_get_side_info_2_single := 17%positive.
Notation IDIII_get_side_info_2_stereo := 18%positive.
Definition III_get_side_info_2 : graph := {|
  g_start := 1%positive;
  g_end := 70%positive;
  g_edges := (1%positive,(AAssign IDIII_get_side_info_2_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDIII_get_side_info_2__tmp
             (Some (EVar IDIII_get_side_info_2_stereo))),3%positive)::
             (3%positive,(AAssign IDIII_get_side_info_2__tmp1
             (Some (EVar IDIII_get_side_info_2_ms_stereo))),4%positive)::
             (4%positive,(AAssign IDIII_get_side_info_2__tmp2
             (Some (EVar IDIII_get_side_info_2_sfreq))),5%positive)::
             (5%positive,(AAssign IDIII_get_side_info_2__tmp3
             (Some (EVar IDIII_get_side_info_2_single))),6%positive)::
             (6%positive,(AAssign IDIII_get_side_info_2_powdiff None),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2__tmp) s) =
             (eval (ENum (1)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2__tmp) s) <>
             (eval (ENum (1)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,13%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDIII_get_side_info_2_ch
             (Some (ENum (0)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2_ch) s) <
             (eval (EVar IDIII_get_side_info_2__tmp) s))%Z)),20%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2_ch) s) >=
             (eval (EVar IDIII_get_side_info_2__tmp) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,70%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (21%positive,ANone,23%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDIII_get_side_info_2_qss None),
             24%positive)::(24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),27%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2__tmp1) s) =
             (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,30%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,46%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDIII_get_side_info_2_i1
             (Some (ENum (0)))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2_i1) s) <
             (eval (ENum (3)) s))%Z)),39%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2_i1) s) >=
             (eval (ENum (3)) s))%Z)),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDIII_get_side_info_2_r0c None),
             37%positive)::
             (37%positive,(AAssign IDIII_get_side_info_2_r1c None),
             38%positive)::(38%positive,ANone,63%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDIII_get_side_info_2_i1
             (Some (EAdd (EVar IDIII_get_side_info_2_i1) (ENum (1))))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDIII_get_side_info_2_z
             (Some (EAdd (ENum (1)) (EVar IDIII_get_side_info_2_z)))),
             45%positive)::(45%positive,AWeaken,34%positive)::
             (46%positive,(AAssign IDIII_get_side_info_2_i
             (Some (ENum (0)))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2_i) s) <
             (eval (ENum (3)) s))%Z)),71%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2_i) s) >=
             (eval (ENum (3)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,69%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,61%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2__tmp2) s) =
             (eval (ENum (8)) s))%Z)),58%positive)::
             (55%positive,(AGuard
             (fun s => ((eval (EVar IDIII_get_side_info_2__tmp2) s) <>
             (eval (ENum (8)) s))%Z)),56%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,60%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,ANone,62%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDIII_get_side_info_2_ch
             (Some (EAdd (EVar IDIII_get_side_info_2_ch) (ENum (1))))),
             65%positive)::(65%positive,ANone,66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDIII_get_side_info_2_z
             (Some (EAdd (ENum (1)) (EVar IDIII_get_side_info_2_z)))),
             68%positive)::(68%positive,AWeaken,16%positive)::
             (69%positive,AWeaken,70%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,(AAssign IDIII_get_side_info_2_sbg None),
             73%positive)::(73%positive,ANone,74%positive)::
             (74%positive,(AAssign IDIII_get_side_info_2_i
             (Some (EAdd (EVar IDIII_get_side_info_2_i) (ENum (1))))),
             75%positive)::(75%positive,ANone,76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,(AAssign IDIII_get_side_info_2_z
             (Some (EAdd (ENum (1)) (EVar IDIII_get_side_info_2_z)))),
             78%positive)::(78%positive,AWeaken,49%positive)::nil
|}.

Definition III_get_side_info_2_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 3%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 4%positive => (1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 5%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 6%positive => (1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 7%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 8%positive => (1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 9%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 10%positive => (1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 11%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp) + -1 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDIII_get_side_info_2__tmp) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp) + -1 <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 13%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 14%positive => (1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 15%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ 1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 16%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 17%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp)+ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 18%positive => (1 * (s IDIII_get_side_info_2__tmp)+ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 19%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp)+ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 20%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 22%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 24%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 26%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp1) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp1) <= 0)%Z
    | 27%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 29%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 31%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) <= 0)%Z
    | 33%positive => (-1 * (s IDIII_get_side_info_2_i1) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -3 <= 0)%Z
    | 35%positive => (1 * (s IDIII_get_side_info_2_i1) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) + 3 <= 0)%Z
    | 36%positive => (-1 * (s IDIII_get_side_info_2_i1) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -3 <= 0)%Z
    | 37%positive => (1 * (s IDIII_get_side_info_2_i1) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) + 3 <= 0)%Z
    | 38%positive => (-1 * (s IDIII_get_side_info_2_i1) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -3 <= 0)%Z
    | 39%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -2 <= 0)%Z
    | 40%positive => (1 * (s IDIII_get_side_info_2_i1) + -2 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 41%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -2 <= 0)%Z
    | 42%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -3 <= 0)%Z
    | 43%positive => (1 * (s IDIII_get_side_info_2_i1) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 44%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i1) + -3 <= 0)%Z
    | 45%positive => (1 * (s IDIII_get_side_info_2_i1) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_i1) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ 1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) <= 0)%Z
    | 48%positive => (-1 * (s IDIII_get_side_info_2_i) <= 0 /\ 1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 50%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 51%positive => (-1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 52%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 53%positive => (-1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 54%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 55%positive => (-1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 56%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 57%positive => (-1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 58%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp2) + -8 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp2) + 8 <= 0)%Z
    | 59%positive => (-1 * (s IDIII_get_side_info_2__tmp2) + 8 <= 0 /\ 1 * (s IDIII_get_side_info_2__tmp2) + -8 <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 60%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 61%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 62%positive => (-1 * (s IDIII_get_side_info_2_i) + 3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0)%Z
    | 63%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 64%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 66%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0)%Z
    | 67%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 68%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) + 1 <= 0)%Z
    | 69%positive => (1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 3 <= 0)%Z
    | 70%positive => (-1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0)%Z
    | 71%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -2 <= 0)%Z
    | 72%positive => (1 * (s IDIII_get_side_info_2_i) + -2 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -2 <= 0)%Z
    | 74%positive => (1 * (s IDIII_get_side_info_2_i) + -2 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_i) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 75%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDIII_get_side_info_2_i) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0)%Z
    | 77%positive => (-1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2_z) <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_i) + 1 <= 0)%Z
    | 78%positive => (-1 * (s IDIII_get_side_info_2_i) + 1 <= 0 /\ 1 * (s IDIII_get_side_info_2_i) + -3 <= 0 /\ -1 * (s IDIII_get_side_info_2_ch) <= 0 /\ -1 * (s IDIII_get_side_info_2__tmp)+ 1 * (s IDIII_get_side_info_2_ch) + 1 <= 0 /\ -1 * (s IDIII_get_side_info_2_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition III_get_side_info_2_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1) * max0((s IDIII_get_side_info_2_stereo)))%Q
    | 2%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2_stereo)))%Q
    | 3%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 4%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 5%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 6%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 7%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 8%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 9%positive => ((s IDIII_get_side_info_2_z)
                     + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 10%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 11%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 12%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 13%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)))%Q
    | 14%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 15%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 16%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 17%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 18%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 19%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 20%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 21%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 22%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 23%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 24%positive => ((4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                     - (s IDIII_get_side_info_2_ch))
                      + max0((s IDIII_get_side_info_2_z)))%Q
    | 25%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 26%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 27%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 28%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 29%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 30%positive => ((4 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 31%positive => ((4 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 32%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 33%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 34%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 35%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 36%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 37%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 38%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 39%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 40%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(2 - (s IDIII_get_side_info_2_i1)))%Q
    | 41%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(2 - (s IDIII_get_side_info_2_i1)))%Q
    | 42%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 43%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 44%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 45%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i1)))%Q
    | 46%positive => ((4 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 47%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + (4 # 3) * max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 48%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + (4 # 3) * max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 49%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 50%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 51%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 52%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 53%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 54%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 55%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 56%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 57%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 58%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 59%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 60%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 61%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 62%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 63%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 64%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 65%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 66%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 67%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 68%positive => ((s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0((s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch)))%Q
    | 69%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 70%positive => ((s IDIII_get_side_info_2_z))%Q
    | 71%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 72%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(2 - (s IDIII_get_side_info_2_i)))%Q
    | 73%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(2 - (s IDIII_get_side_info_2_i)))%Q
    | 74%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(2 - (s IDIII_get_side_info_2_i)))%Q
    | 75%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 76%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 77%positive => ((2 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | 78%positive => ((1 # 1) + (s IDIII_get_side_info_2_z)
                      + (4 # 1) * max0(-1 + (s IDIII_get_side_info_2__tmp)
                                       - (s IDIII_get_side_info_2_ch))
                      + max0(3 - (s IDIII_get_side_info_2_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition III_get_side_info_2_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDIII_get_side_info_2_z)) (0))) (F_max0_ge_0 ((s IDIII_get_side_info_2_z)))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-4 0*) F_max0_monotonic (F_check_ge ((s IDIII_get_side_info_2__tmp)
                                                             - (s IDIII_get_side_info_2_ch)) (-1
                                                                    + (s IDIII_get_side_info_2__tmp)
                                                                    - (s IDIII_get_side_info_2_ch)));
                      (*-4 0*) F_max0_ge_0 (-1
                                            + (s IDIII_get_side_info_2__tmp)
                                            - (s IDIII_get_side_info_2_ch));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDIII_get_side_info_2_z))) (F_check_ge ((s IDIII_get_side_info_2_z)) (0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDIII_get_side_info_2_z))) (F_check_ge ((s IDIII_get_side_info_2_z)) (0))]
    | 25%positive => []
    | 26%positive => [(*-4 0*) F_max0_pre_decrement ((s IDIII_get_side_info_2__tmp)
                                                     - (s IDIII_get_side_info_2_ch)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-4 0*) F_max0_pre_decrement ((s IDIII_get_side_info_2__tmp)
                                                     - (s IDIII_get_side_info_2_ch)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDIII_get_side_info_2_i1)) (2
                                                                    - (s IDIII_get_side_info_2_i1)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDIII_get_side_info_2_i1))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-1 0*) F_max0_pre_decrement (3
                                                     - (s IDIII_get_side_info_2_i1)) (1)]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-0.333333 0*) F_max0_monotonic (F_check_ge (3
                                                                    - 
                                                                    (s IDIII_get_side_info_2_i)) (3))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDIII_get_side_info_2_i)) (2
                                                                    - (s IDIII_get_side_info_2_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDIII_get_side_info_2_i))]
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
    | 68%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDIII_get_side_info_2_z)) (0))) (F_max0_ge_0 ((s IDIII_get_side_info_2_z)))]
    | 69%positive => [(*-1 0*) F_one;
                      (*-4 0*) F_max0_ge_0 (-1
                                            + (s IDIII_get_side_info_2__tmp)
                                            - (s IDIII_get_side_info_2_ch));
                      (*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDIII_get_side_info_2_i)) (2
                                                                    - (s IDIII_get_side_info_2_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDIII_get_side_info_2_i))]
    | 70%positive => []
    | 71%positive => [(*-1 0*) F_max0_pre_decrement (3
                                                     - (s IDIII_get_side_info_2_i)) (1)]
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | _ => []
  end.


Theorem III_get_side_info_2_ai_correct:
  forall s p' s', steps (g_start III_get_side_info_2) s (g_edges III_get_side_info_2) p' s' -> III_get_side_info_2_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem III_get_side_info_2_pot_correct:
  forall s p' s',
    steps (g_start III_get_side_info_2) s (g_edges III_get_side_info_2) p' s' ->
    (III_get_side_info_2_pot (g_start III_get_side_info_2) s >= III_get_side_info_2_pot p' s')%Q.
Proof.
  check_lp III_get_side_info_2_ai_correct III_get_side_info_2_hints.
Qed.

