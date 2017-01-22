Require Import pasta.Pasta.

Notation IDzreadline_from_z := 1%positive.
Notation IDzreadline_from__tmp := 2%positive.
Notation IDzreadline_from__tmp1 := 3%positive.
Notation IDzreadline_from_ch := 4%positive.
Notation IDzreadline_from_ch1 := 5%positive.
Notation IDzreadline_from_count := 6%positive.
Notation IDzreadline_from_pcount_dref := 7%positive.
Notation IDzreadline_from_pin_eol_dref := 8%positive.
Notation IDzreadline_from_pcount := 9%positive.
Notation IDzreadline_from_pin_eol := 10%positive.
Notation IDzreadline_from_ptr := 11%positive.
Notation IDzreadline_from_s := 12%positive.
Notation IDzreadline_from_size := 13%positive.
Definition zreadline_from : graph := {|
  g_start := 1%positive;
  g_end := 75%positive;
  g_edges := (1%positive,(AAssign IDzreadline_from_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_count) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from__tmp1) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDzreadline_from__tmp1
             (Some (EVar IDzreadline_from_size))),6%positive)::
             (6%positive,(AAssign IDzreadline_from_count
             (Some (EVar IDzreadline_from_pcount_dref))),7%positive)::
             (7%positive,ANone,8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_pin_eol_dref) s) <>
             (eval (ENum (0)) s))%Z)),49%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_pin_eol_dref) s) =
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,15%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,16%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDzreadline_from_ch1 None),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_ch1) s) <
             (eval (ENum (0)) s))%Z)),44%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_ch1) s) >=
             (eval (ENum (0)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,31%positive)::
             (20%positive,ANone,25%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDzreadline_from_pcount_dref
             (Some (EVar IDzreadline_from_count))),22%positive)::
             (22%positive,(AAssign IDzreadline_from__tmp (Some (ENum (0)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,75%positive)::
             (25%positive,(AAssign IDzreadline_from_pcount_dref
             (Some (EVar IDzreadline_from_count))),26%positive)::
             (26%positive,(AAssign IDzreadline_from_pin_eol_dref
             (Some (ENum (1)))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDzreadline_from_z (Some (EAdd (ENum (1))
             (EVar IDzreadline_from_z)))),30%positive)::
             (30%positive,AWeaken,9%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_count) s) >=
             (eval (EVar IDzreadline_from__tmp1) s))%Z)),39%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDzreadline_from_count) s) <
             (eval (EVar IDzreadline_from__tmp1) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDzreadline_from_count
             (Some (EAdd (EVar IDzreadline_from_count) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDzreadline_from_z (Some (EAdd (ENum (1))
             (EVar IDzreadline_from_z)))),38%positive)::
             (38%positive,AWeaken,13%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDzreadline_from_pcount_dref
             (Some (EVar IDzreadline_from_count))),41%positive)::
             (41%positive,(AAssign IDzreadline_from__tmp (Some (ENum (1)))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,AWeaken,75%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDzreadline_from_pcount_dref
             (Some (EVar IDzreadline_from_count))),46%positive)::
             (46%positive,(AAssign IDzreadline_from__tmp
             (Some (EVar IDzreadline_from_ch1))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,75%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AAssign IDzreadline_from_ch None),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard (fun s => ((eval (EVar IDzreadline_from_ch)
             s) = (eval (ENum (-1)) s))%Z)),70%positive)::
             (52%positive,(AGuard (fun s => ((eval (EVar IDzreadline_from_ch)
             s) <> (eval (ENum (-1)) s))%Z)),53%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AGuard (fun s => ((eval (EVar IDzreadline_from_ch)
             s) < (eval (ENum (0)) s))%Z)),66%positive)::
             (54%positive,(AGuard (fun s => ((eval (EVar IDzreadline_from_ch)
             s) >= (eval (ENum (0)) s))%Z)),55%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,(AGuard (fun s => ((eval (EVar IDzreadline_from_ch)
             s) <> (eval (ENum (10)) s))%Z)),58%positive)::
             (56%positive,(AGuard (fun s => ((eval (EVar IDzreadline_from_ch)
             s) = (eval (ENum (10)) s))%Z)),57%positive)::
             (57%positive,AWeaken,60%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,(AAssign IDzreadline_from_pin_eol_dref
             (Some (ENum (0)))),63%positive)::
             (63%positive,(AAssign IDzreadline_from__tmp (Some (ENum (0)))),
             64%positive)::(64%positive,ANone,65%positive)::
             (65%positive,AWeaken,75%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,(AAssign IDzreadline_from__tmp
             (Some (EVar IDzreadline_from_ch))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,AWeaken,75%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,(AAssign IDzreadline_from_pin_eol_dref
             (Some (ENum (0)))),72%positive)::
             (72%positive,(AAssign IDzreadline_from__tmp (Some (ENum (0)))),
             73%positive)::(73%positive,ANone,74%positive)::
             (74%positive,AWeaken,75%positive)::nil
|}.

Definition zreadline_from_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_count) <= 0)%Z
    | 4%positive => (-1 * (s IDzreadline_from_count) <= 0 /\ 1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from__tmp1) <= 0)%Z
    | 5%positive => (-1 * (s IDzreadline_from__tmp1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_count) <= 0)%Z
    | 6%positive => (-1 * (s IDzreadline_from_count) <= 0 /\ 1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_z) <= 0)%Z
    | 8%positive => (1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 9%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 10%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 11%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 12%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 13%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 14%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 15%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 16%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 17%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 18%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 19%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0)%Z
    | 20%positive => (-1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 21%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0)%Z
    | 22%positive => (-1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 23%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from__tmp) <= 0 /\ -1 * (s IDzreadline_from__tmp) <= 0)%Z
    | 24%positive => (-1 * (s IDzreadline_from__tmp) <= 0 /\ 1 * (s IDzreadline_from__tmp) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 25%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0)%Z
    | 26%positive => (-1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 27%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) + -1 <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDzreadline_from_pin_eol_dref) + 1 <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) + -1 <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 29%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) + -1 <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDzreadline_from_pin_eol_dref) + 1 <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) + -1 <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0)%Z
    | 32%positive => (-1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 33%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from__tmp1)+ 1 * (s IDzreadline_from_count) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDzreadline_from__tmp1)+ 1 * (s IDzreadline_from_count) + 1 <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 35%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from__tmp1)+ 1 * (s IDzreadline_from_count) <= 0)%Z
    | 36%positive => (-1 * (s IDzreadline_from__tmp1)+ 1 * (s IDzreadline_from_count) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 37%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from__tmp1)+ 1 * (s IDzreadline_from_count) <= 0)%Z
    | 38%positive => (-1 * (s IDzreadline_from__tmp1)+ 1 * (s IDzreadline_from_count) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_count) <= 0)%Z
    | 40%positive => (1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_count) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 41%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_count) <= 0 /\ 1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_pcount_dref) <= 0)%Z
    | 42%positive => (1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_pcount_dref) <= 0 /\ 1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_count) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from__tmp) + -1 <= 0 /\ -1 * (s IDzreadline_from__tmp) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDzreadline_from__tmp) + 1 <= 0 /\ 1 * (s IDzreadline_from__tmp) + -1 <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch1) <= 0 /\ 1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_count) <= 0 /\ 1 * (s IDzreadline_from__tmp1)+ -1 * (s IDzreadline_from_pcount_dref) <= 0)%Z
    | 44%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch1) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDzreadline_from_ch1) + 1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 46%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch1) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDzreadline_from_ch1) + 1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from__tmp) + 1 <= 0)%Z
    | 48%positive => (1 * (s IDzreadline_from__tmp) + 1 <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch1) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 50%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 51%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 52%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 53%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 54%positive => (-1 * (s IDzreadline_from_z) <= 0)%Z
    | 55%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch) <= 0)%Z
    | 56%positive => (-1 * (s IDzreadline_from_ch) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 57%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch) + -10 <= 0 /\ -1 * (s IDzreadline_from_ch) + 10 <= 0)%Z
    | 58%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch) <= 0)%Z
    | 59%positive => (-1 * (s IDzreadline_from_ch) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 60%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch) <= 0)%Z
    | 61%positive => (-1 * (s IDzreadline_from_ch) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 62%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch) <= 0)%Z
    | 63%positive => (-1 * (s IDzreadline_from_ch) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 64%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ -1 * (s IDzreadline_from_ch) <= 0 /\ 1 * (s IDzreadline_from__tmp) <= 0 /\ -1 * (s IDzreadline_from__tmp) <= 0)%Z
    | 65%positive => (-1 * (s IDzreadline_from__tmp) <= 0 /\ 1 * (s IDzreadline_from__tmp) <= 0 /\ -1 * (s IDzreadline_from_ch) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 66%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0)%Z
    | 67%positive => (1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 68%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ 1 * (s IDzreadline_from__tmp) + 1 <= 0)%Z
    | 69%positive => (1 * (s IDzreadline_from__tmp) + 1 <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 70%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_ch) + -1 <= 0)%Z
    | 71%positive => (-1 * (s IDzreadline_from_ch) + -1 <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | 72%positive => (-1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_ch) + -1 <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 73%positive => (-1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_ch) + -1 <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from__tmp) <= 0 /\ -1 * (s IDzreadline_from__tmp) <= 0)%Z
    | 74%positive => (-1 * (s IDzreadline_from__tmp) <= 0 /\ 1 * (s IDzreadline_from__tmp) <= 0 /\ -1 * (s IDzreadline_from_z) <= 0 /\ 1 * (s IDzreadline_from_ch) + 1 <= 0 /\ -1 * (s IDzreadline_from_ch) + -1 <= 0 /\ 1 * (s IDzreadline_from_pin_eol_dref) <= 0 /\ -1 * (s IDzreadline_from_pin_eol_dref) <= 0)%Z
    | 75%positive => (1 * (s IDzreadline_from__tmp) + -1 <= 0 /\ -1 * (s IDzreadline_from_z) <= 0)%Z
    | _ => False
  end.

Definition zreadline_from_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0(-(s IDzreadline_from_pcount_dref)
                            + (s IDzreadline_from_size)))%Q
    | 2%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0(-(s IDzreadline_from_pcount_dref)
                            + (s IDzreadline_from_size)))%Q
    | 3%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0(-(s IDzreadline_from_pcount_dref)
                            + (s IDzreadline_from_size)))%Q
    | 4%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0(-(s IDzreadline_from_pcount_dref)
                            + (s IDzreadline_from_size)))%Q
    | 5%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0(-(s IDzreadline_from_pcount_dref)
                            + (s IDzreadline_from_size)))%Q
    | 6%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0((s IDzreadline_from__tmp1)
                            - (s IDzreadline_from_pcount_dref)))%Q
    | 7%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0((s IDzreadline_from__tmp1)
                            - (s IDzreadline_from_count)))%Q
    | 8%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0((s IDzreadline_from__tmp1)
                            - (s IDzreadline_from_count)))%Q
    | 9%positive => ((s IDzreadline_from_z)
                     + max0(1 - (s IDzreadline_from_pin_eol_dref))
                     + max0((s IDzreadline_from__tmp1)
                            - (s IDzreadline_from_count)))%Q
    | 10%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 11%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 12%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 13%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 14%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 15%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 16%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 17%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 18%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 19%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 20%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 21%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 22%positive => ((1 # 1) + (s IDzreadline_from_z)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 23%positive => ((1 # 1) + (s IDzreadline_from_z)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 24%positive => ((1 # 1) + (s IDzreadline_from_z)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 25%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 26%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 27%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 28%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 29%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 30%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 31%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 32%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 33%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 34%positive => ((2 # 1) + (s IDzreadline_from_z)
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 35%positive => ((2 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 36%positive => ((2 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 37%positive => ((2 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 38%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 39%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 40%positive => ((1 # 1) + (s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 41%positive => ((1 # 1) + (s IDzreadline_from_z)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 42%positive => ((1 # 1) + (s IDzreadline_from_z)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 43%positive => ((1 # 1) + (s IDzreadline_from_z)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 44%positive => (max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0((s IDzreadline_from_z)))%Q
    | 45%positive => ((s IDzreadline_from_pin_eol_dref)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-(s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from_z)))%Q
    | 46%positive => ((s IDzreadline_from_pin_eol_dref)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-(s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from_z)))%Q
    | 47%positive => ((s IDzreadline_from_pin_eol_dref)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-(s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from_z)))%Q
    | 48%positive => ((s IDzreadline_from_pin_eol_dref)
                      - max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-1 + (s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_pcount_dref))
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count))
                      + max0(-(s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from_z)))%Q
    | 49%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 50%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 51%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 52%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 53%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 54%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 55%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 56%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 57%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 58%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 59%positive => ((s IDzreadline_from_z))%Q
    | 60%positive => ((s IDzreadline_from_z))%Q
    | 61%positive => ((s IDzreadline_from_z))%Q
    | 62%positive => ((s IDzreadline_from_z))%Q
    | 63%positive => ((s IDzreadline_from_z))%Q
    | 64%positive => ((s IDzreadline_from_z))%Q
    | 65%positive => ((s IDzreadline_from_z))%Q
    | 66%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 67%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 68%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 69%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 70%positive => ((s IDzreadline_from_z)
                      + max0(1 - (s IDzreadline_from_pin_eol_dref))
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 71%positive => ((s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 72%positive => ((s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 73%positive => ((s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 74%positive => ((s IDzreadline_from_z)
                      + max0((s IDzreadline_from__tmp1)
                             - (s IDzreadline_from_count)))%Q
    | 75%positive => ((s IDzreadline_from_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zreadline_from_hints (p : node) (s : state) := 
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
    | 10%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzreadline_from_z)) (0))) (F_max0_ge_0 ((s IDzreadline_from_z)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzreadline_from_z))) (F_check_ge ((s IDzreadline_from_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzreadline_from_pin_eol_dref)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzreadline_from_pin_eol_dref)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   - 
                                                                   (s IDzreadline_from_pin_eol_dref))) (F_check_ge (1
                                                                    - (s IDzreadline_from_pin_eol_dref)) (0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                             - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzreadline_from_pin_eol_dref)) (0))) (F_max0_ge_0 ((s IDzreadline_from_pin_eol_dref)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDzreadline_from_pin_eol_dref)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDzreadline_from_pin_eol_dref)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzreadline_from__tmp1)
                                                                 - (s IDzreadline_from_pcount_dref))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_pre_decrement ((s IDzreadline_from__tmp1)
                                                     - (s IDzreadline_from_count)) (1)]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzreadline_from_z)) (0))) (F_max0_ge_0 ((s IDzreadline_from_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzreadline_from_pin_eol_dref)) (0))) (F_max0_ge_0 ((s IDzreadline_from_pin_eol_dref)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDzreadline_from_pin_eol_dref)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDzreadline_from_pin_eol_dref)))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                             - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzreadline_from__tmp1)
                                                                 - (s IDzreadline_from_pcount_dref))) (F_check_ge (0) (0))]
    | 44%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDzreadline_from_pin_eol_dref)) (0))) (F_max0_ge_0 (-
                                                                    (s IDzreadline_from_pin_eol_dref)))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                             - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzreadline_from_z))) (F_check_ge ((s IDzreadline_from_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   - 
                                                                   (s IDzreadline_from_pin_eol_dref))) (F_check_ge (1
                                                                    - (s IDzreadline_from_pin_eol_dref)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzreadline_from__tmp1)
                                                                 - (s IDzreadline_from_pcount_dref))) (F_check_ge (0) (0))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                            - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzreadline_from__tmp1)
                                                                 - (s IDzreadline_from_count))) (F_check_ge (0) (0))]
    | 58%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                             - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzreadline_from__tmp1)
                                                                 - (s IDzreadline_from_count))) (F_check_ge (0) (0))]
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
    | 69%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                             - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDzreadline_from__tmp1)
                                            - (s IDzreadline_from_count));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0))]
    | 70%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDzreadline_from_pin_eol_dref))) (F_check_ge (0) (0))]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDzreadline_from__tmp1)
                                                             - (s IDzreadline_from_count)) (-1
                                                                    + (s IDzreadline_from__tmp1)
                                                                    - (s IDzreadline_from_count)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDzreadline_from__tmp1)
                                            - (s IDzreadline_from_count))]
    | 75%positive => []
    | _ => []
  end.


Theorem zreadline_from_ai_correct:
  forall s p' s', steps (g_start zreadline_from) s (g_edges zreadline_from) p' s' -> zreadline_from_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zreadline_from_pot_correct:
  forall s p' s',
    steps (g_start zreadline_from) s (g_edges zreadline_from) p' s' ->
    (zreadline_from_pot (g_start zreadline_from) s >= zreadline_from_pot p' s')%Q.
Proof.
  check_lp zreadline_from_ai_correct zreadline_from_hints.
Qed.

