Require Import pasta.Pasta.

Notation IDadd_tag_z := 1%positive.
Notation IDadd_tag__tmp := 2%positive.
Notation IDadd_tag__tmp1 := 3%positive.
Notation IDadd_tag_begin1 := 4%positive.
Notation IDadd_tag_begin2 := 5%positive.
Notation IDadd_tag_end1 := 6%positive.
Notation IDadd_tag_end2 := 7%positive.
Notation IDadd_tag_file_dref_off32 := 8%positive.
Notation IDadd_tag_i := 9%positive.
Notation IDadd_tag_location := 10%positive.
Notation IDadd_tag_file := 11%positive.
Notation IDadd_tag_length := 12%positive.
Definition add_tag : graph := {|
  g_start := 1%positive;
  g_end := 71%positive;
  g_edges := (1%positive,(AAssign IDadd_tag_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDadd_tag_file_dref_off32) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_end2) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_end1) s) >=
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_begin2)
             s) >= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_begin1)
             s) >= (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDadd_tag__tmp1
             (Some (EVar IDadd_tag_length))),10%positive)::
             (10%positive,(AAssign IDadd_tag_location None),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_location)
             s) = (eval (ENum (-1)) s))%Z)),67%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_location)
             s) <> (eval (ENum (-1)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDadd_tag_begin1
             (Some (EVar IDadd_tag_location))),15%positive)::
             (15%positive,(AAssign IDadd_tag_end1
             (Some (EAdd (EVar IDadd_tag_begin1) (EVar IDadd_tag__tmp1)))),
             16%positive)::
             (16%positive,(AAssign IDadd_tag_i (Some (ENum (0)))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_i) s) <
             (eval (EVar IDadd_tag_file_dref_off32) s))%Z)),39%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_i) s) >=
             (eval (EVar IDadd_tag_file_dref_off32) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,36%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,33%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,30%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDadd_tag_file_dref_off32
             (Some (EAdd (EVar IDadd_tag_file_dref_off32) (ENum (1))))),
             27%positive)::
             (27%positive,(AAssign IDadd_tag__tmp (Some (ENum (0)))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,AWeaken,71%positive)::
             (30%positive,(AAssign IDadd_tag__tmp (Some (ENum (-1)))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,AWeaken,71%positive)::
             (33%positive,(AAssign IDadd_tag__tmp (Some (ENum (-1)))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,AWeaken,71%positive)::
             (36%positive,(AAssign IDadd_tag__tmp (Some (ENum (-1)))),
             37%positive)::(37%positive,ANone,38%positive)::
             (38%positive,AWeaken,71%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDadd_tag_begin2 None),41%positive)::
             (41%positive,(AAssign IDadd_tag_end2 None),42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_begin1)
             s) = (eval (EVar IDadd_tag_begin2) s))%Z)),45%positive)::
             (43%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_begin1)
             s) <> (eval (EVar IDadd_tag_begin2) s))%Z)),44%positive)::
             (44%positive,AWeaken,48%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_end1) s) =
             (eval (EVar IDadd_tag_end2) s))%Z)),63%positive)::
             (46%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_end1)
             s) <> (eval (EVar IDadd_tag_end2) s))%Z)),47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_begin1)
             s) < (eval (EVar IDadd_tag_end2) s))%Z)),50%positive)::
             (48%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_begin1)
             s) >= (eval (EVar IDadd_tag_end2) s))%Z)),49%positive)::
             (49%positive,AWeaken,53%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_end1) s) >
             (eval (EVar IDadd_tag_begin2) s))%Z)),59%positive)::
             (51%positive,(AGuard (fun s => ((eval (EVar IDadd_tag_end1)
             s) <= (eval (EVar IDadd_tag_begin2) s))%Z)),52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDadd_tag_i (Some (EAdd (EVar IDadd_tag_i)
             (ENum (1))))),55%positive)::(55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDadd_tag_z (Some (EAdd (ENum (1))
             (EVar IDadd_tag_z)))),58%positive)::
             (58%positive,AWeaken,19%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AAssign IDadd_tag__tmp (Some (ENum (-1)))),
             61%positive)::(61%positive,ANone,62%positive)::
             (62%positive,AWeaken,71%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDadd_tag__tmp (Some (ENum (0)))),
             65%positive)::(65%positive,ANone,66%positive)::
             (66%positive,AWeaken,71%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,(AAssign IDadd_tag__tmp (Some (ENum (-1)))),
             69%positive)::(69%positive,ANone,70%positive)::
             (70%positive,AWeaken,71%positive)::nil
|}.

Definition add_tag_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0)%Z
    | 3%positive => (-1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0)%Z
    | 4%positive => (-1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0)%Z
    | 5%positive => (-1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0)%Z
    | 6%positive => (-1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0)%Z
    | 7%positive => (-1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0)%Z
    | 8%positive => (-1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_begin1) <= 0)%Z
    | 9%positive => (-1 * (s IDadd_tag_begin1) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0)%Z
    | 10%positive => (-1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_begin1) <= 0)%Z
    | 11%positive => (-1 * (s IDadd_tag_begin1) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0)%Z
    | 12%positive => (-1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_begin1) <= 0)%Z
    | 13%positive => (-1 * (s IDadd_tag_begin1) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0)%Z
    | 14%positive => (-1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_begin1) <= 0)%Z
    | 15%positive => (-1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0)%Z
    | 16%positive => (-1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0)%Z
    | 17%positive => (-1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0)%Z
    | 18%positive => (-1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0)%Z
    | 19%positive => (-1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0)%Z
    | 20%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 21%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0)%Z
    | 22%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 23%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0)%Z
    | 24%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 25%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0)%Z
    | 26%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 27%positive => (-1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) + -1 <= 0)%Z
    | 28%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) + -1 <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag__tmp) <= 0 /\ -1 * (s IDadd_tag__tmp) <= 0)%Z
    | 29%positive => (-1 * (s IDadd_tag__tmp) <= 0 /\ 1 * (s IDadd_tag__tmp) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) + -1 <= 0)%Z
    | 30%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 31%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag__tmp) + -1 <= 0)%Z
    | 32%positive => (-1 * (s IDadd_tag__tmp) + -1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 33%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 34%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag__tmp) + -1 <= 0)%Z
    | 35%positive => (-1 * (s IDadd_tag__tmp) + -1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 36%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 37%positive => (1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag__tmp) + -1 <= 0)%Z
    | 38%positive => (-1 * (s IDadd_tag__tmp) + -1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_file_dref_off32)+ -1 * (s IDadd_tag_i) <= 0)%Z
    | 39%positive => (-1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0)%Z
    | 41%positive => (-1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0)%Z
    | 43%positive => (-1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0)%Z
    | 45%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0)%Z
    | 46%positive => (-1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0)%Z
    | 48%positive => (-1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_end2) <= 0)%Z
    | 50%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0 /\ -1 * (s IDadd_tag_begin2)+ 1 * (s IDadd_tag_end1) <= 0)%Z
    | 53%positive => (-1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0)%Z
    | 55%positive => (-1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0)%Z
    | 57%positive => (-1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0 /\ 1 * (s IDadd_tag_begin2)+ -1 * (s IDadd_tag_end1) + 1 <= 0)%Z
    | 60%positive => (1 * (s IDadd_tag_begin2)+ -1 * (s IDadd_tag_end1) + 1 <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0 /\ 1 * (s IDadd_tag_begin2)+ -1 * (s IDadd_tag_end1) + 1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag__tmp) + -1 <= 0)%Z
    | 62%positive => (-1 * (s IDadd_tag__tmp) + -1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ 1 * (s IDadd_tag_begin2)+ -1 * (s IDadd_tag_end1) + 1 <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_end2) + 1 <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_end1)+ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_end1)+ 1 * (s IDadd_tag_end2) <= 0)%Z
    | 64%positive => (-1 * (s IDadd_tag_end1)+ 1 * (s IDadd_tag_end2) <= 0 /\ 1 * (s IDadd_tag_end1)+ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_end1)+ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_end1)+ 1 * (s IDadd_tag_end2) <= 0 /\ 1 * (s IDadd_tag__tmp) <= 0 /\ -1 * (s IDadd_tag__tmp) <= 0)%Z
    | 66%positive => (-1 * (s IDadd_tag__tmp) <= 0 /\ 1 * (s IDadd_tag__tmp) <= 0 /\ -1 * (s IDadd_tag_end1)+ 1 * (s IDadd_tag_end2) <= 0 /\ 1 * (s IDadd_tag_end1)+ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin1)+ 1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_begin1)+ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32)+ 1 * (s IDadd_tag_i) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDadd_tag_begin1) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_location) + 1 <= 0 /\ -1 * (s IDadd_tag_location) + -1 <= 0)%Z
    | 68%positive => (-1 * (s IDadd_tag_location) + -1 <= 0 /\ 1 * (s IDadd_tag_location) + 1 <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_begin1) <= 0)%Z
    | 69%positive => (-1 * (s IDadd_tag_begin1) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0 /\ 1 * (s IDadd_tag_location) + 1 <= 0 /\ -1 * (s IDadd_tag_location) + -1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag__tmp) + -1 <= 0)%Z
    | 70%positive => (-1 * (s IDadd_tag__tmp) + -1 <= 0 /\ 1 * (s IDadd_tag__tmp) + 1 <= 0 /\ -1 * (s IDadd_tag_location) + -1 <= 0 /\ 1 * (s IDadd_tag_location) + 1 <= 0 /\ -1 * (s IDadd_tag_begin2) <= 0 /\ -1 * (s IDadd_tag_end2) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ 1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_end1) <= 0 /\ -1 * (s IDadd_tag_begin1) <= 0)%Z
    | 71%positive => (1 * (s IDadd_tag__tmp) <= 0 /\ -1 * (s IDadd_tag_file_dref_off32) <= 0 /\ -1 * (s IDadd_tag_z) <= 0 /\ -1 * (s IDadd_tag_i) <= 0 /\ -1 * (s IDadd_tag__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition add_tag_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 2%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 3%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 4%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 5%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 6%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 7%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 8%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 9%positive => (-(s IDadd_tag_file_dref_off32)
                     + (2 # 1) * max0((s IDadd_tag_file_dref_off32)))%Q
    | 10%positive => (-(s IDadd_tag_file_dref_off32)
                      + (2 # 1) * max0((s IDadd_tag_file_dref_off32)))%Q
    | 11%positive => (-(s IDadd_tag_file_dref_off32)
                      + (2 # 1) * max0((s IDadd_tag_file_dref_off32)))%Q
    | 12%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 13%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 14%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 15%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 16%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 17%positive => (max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 18%positive => (max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 19%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 20%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 21%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 22%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 23%positive => ((s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 24%positive => ((s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 25%positive => ((s IDadd_tag_z))%Q
    | 26%positive => ((s IDadd_tag_z))%Q
    | 27%positive => ((s IDadd_tag_z))%Q
    | 28%positive => ((s IDadd_tag_z))%Q
    | 29%positive => ((s IDadd_tag_z))%Q
    | 30%positive => ((s IDadd_tag_z))%Q
    | 31%positive => ((s IDadd_tag_z))%Q
    | 32%positive => ((s IDadd_tag_z))%Q
    | 33%positive => ((s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 34%positive => ((s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 35%positive => ((s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 36%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 37%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 38%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 39%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 40%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 41%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 42%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 43%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 44%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 45%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 46%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 47%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 48%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 49%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 50%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 51%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 52%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 53%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 54%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 55%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 56%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 57%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 58%positive => ((s IDadd_tag_z)
                      + max0((s IDadd_tag_file_dref_off32) - (s IDadd_tag_i)))%Q
    | 59%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 60%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 61%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 62%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 63%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 64%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 65%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 66%positive => ((1 # 1) + (s IDadd_tag_z)
                      + max0(-1 + (s IDadd_tag_file_dref_off32)
                             - (s IDadd_tag_i)))%Q
    | 67%positive => (max0((s IDadd_tag_file_dref_off32)))%Q
    | 68%positive => ((s IDadd_tag_z) + max0((s IDadd_tag_file_dref_off32))
                      + max0(-(s IDadd_tag_z)))%Q
    | 69%positive => ((s IDadd_tag_z) + max0((s IDadd_tag_file_dref_off32))
                      + max0(-(s IDadd_tag_z)))%Q
    | 70%positive => ((s IDadd_tag_z) + max0((s IDadd_tag_file_dref_off32))
                      + max0(-(s IDadd_tag_z)))%Q
    | 71%positive => ((s IDadd_tag_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition add_tag_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDadd_tag_file_dref_off32)) (0))) (F_max0_ge_0 ((s IDadd_tag_file_dref_off32)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadd_tag_file_dref_off32))) (F_check_ge ((s IDadd_tag_file_dref_off32)) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDadd_tag_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDadd_tag_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDadd_tag_z)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDadd_tag_file_dref_off32)
                                                            - (s IDadd_tag_i)) (-1
                                                                    + (s IDadd_tag_file_dref_off32)
                                                                    - (s IDadd_tag_i)))]
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDadd_tag_file_dref_off32)
                                            - (s IDadd_tag_i))]
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
    | 35%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDadd_tag_file_dref_off32)
                                            - (s IDadd_tag_i))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDadd_tag_file_dref_off32)
                                                             - (s IDadd_tag_i)) (-1
                                                                    + (s IDadd_tag_file_dref_off32)
                                                                    - (s IDadd_tag_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDadd_tag_file_dref_off32)
                                            - (s IDadd_tag_i))]
    | 39%positive => [(*0 1*) F_max0_pre_decrement ((s IDadd_tag_file_dref_off32)
                                                    - (s IDadd_tag_i)) (1)]
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
    | 58%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDadd_tag_z))) (F_check_ge ((s IDadd_tag_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDadd_tag_z)) (0))) (F_max0_ge_0 ((s IDadd_tag_z)))]
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDadd_tag_file_dref_off32)
                                            - (s IDadd_tag_i))]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDadd_tag_file_dref_off32)
                                            - (s IDadd_tag_i))]
    | 67%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDadd_tag_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDadd_tag_z)))]
    | 68%positive => []
    | 69%positive => []
    | 70%positive => [(*-1 0*) F_max0_ge_0 ((s IDadd_tag_file_dref_off32));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDadd_tag_z))) (F_check_ge (0) (0))]
    | 71%positive => []
    | _ => []
  end.


Theorem add_tag_ai_correct:
  forall s p' s', steps (g_start add_tag) s (g_edges add_tag) p' s' -> add_tag_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem add_tag_pot_correct:
  forall s p' s',
    steps (g_start add_tag) s (g_edges add_tag) p' s' ->
    (add_tag_pot (g_start add_tag) s >= add_tag_pot p' s')%Q.
Proof.
  check_lp add_tag_ai_correct add_tag_hints.
Qed.

