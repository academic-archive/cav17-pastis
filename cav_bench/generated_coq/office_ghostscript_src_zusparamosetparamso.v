Require Import pasta.Pasta.

Notation IDsetparams_z := 1%positive.
Notation IDsetparams__tmp := 2%positive.
Notation IDsetparams_code := 3%positive.
Notation IDsetparams_i := 4%positive.
Notation IDsetparams_pset_dref_off24 := 5%positive.
Notation IDsetparams_pset_dref_off8 := 6%positive.
Notation IDsetparams_plist := 7%positive.
Notation IDsetparams_pset := 8%positive.
Definition setparams : graph := {|
  g_start := 1%positive;
  g_end := 68%positive;
  g_edges := (1%positive,(AAssign IDsetparams_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDsetparams_pset_dref_off8) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDsetparams_pset_dref_off24) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDsetparams_i) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDsetparams_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDsetparams_i) s) <
             (eval (EVar IDsetparams_pset_dref_off8) s))%Z)),42%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDsetparams_i) s) >=
             (eval (EVar IDsetparams_pset_dref_off8) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDsetparams_i (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDsetparams_i) s) <
             (eval (EVar IDsetparams_pset_dref_off24) s))%Z)),19%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDsetparams_i) s) >=
             (eval (EVar IDsetparams_pset_dref_off24) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDsetparams__tmp (Some (ENum (0)))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,AWeaken,68%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,36%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDsetparams_code None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDsetparams_code)
             s) = (eval (ENum (0)) s))%Z)),25%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDsetparams_code)
             s) <> (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,29%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDsetparams_code None),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDsetparams_code)
             s) < (eval (ENum (0)) s))%Z)),32%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDsetparams_code)
             s) >= (eval (ENum (0)) s))%Z)),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,37%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDsetparams__tmp
             (Some (EVar IDsetparams_code))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,68%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDsetparams_i
             (Some (EAdd (EVar IDsetparams_i) (ENum (1))))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDsetparams_z (Some (EAdd (ENum (1))
             (EVar IDsetparams_z)))),41%positive)::
             (41%positive,AWeaken,14%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,69%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDsetparams_code None),45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,65%positive)::
             (46%positive,ANone,63%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,60%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,60%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDsetparams_code None),52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDsetparams_code)
             s) < (eval (ENum (0)) s))%Z)),56%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDsetparams_code)
             s) >= (eval (ENum (0)) s))%Z)),54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,64%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AAssign IDsetparams__tmp
             (Some (EVar IDsetparams_code))),58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,68%positive)::
             (60%positive,(AAssign IDsetparams__tmp (Some (ENum (-15)))),
             61%positive)::(61%positive,ANone,62%positive)::
             (62%positive,AWeaken,68%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,70%positive)::
             (65%positive,(AAssign IDsetparams__tmp
             (Some (EVar IDsetparams_code))),66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,AWeaken,68%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDsetparams_i
             (Some (EAdd (EVar IDsetparams_i) (ENum (1))))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDsetparams_z (Some (EAdd (ENum (1))
             (EVar IDsetparams_z)))),74%positive)::
             (74%positive,AWeaken,9%positive)::nil
|}.

Definition setparams_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off8) <= 0)%Z
    | 4%positive => (-1 * (s IDsetparams_pset_dref_off8) <= 0 /\ 1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 5%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 6%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ 1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 7%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ 1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 8%positive => (-1 * (s IDsetparams_i) <= 0 /\ 1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ 1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 9%positive => (-1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0)%Z
    | 10%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i)+ 1 * (s IDsetparams_pset_dref_off8) <= 0)%Z
    | 11%positive => (-1 * (s IDsetparams_i)+ 1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0)%Z
    | 12%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 13%positive => (-1 * (s IDsetparams_i) <= 0 /\ 1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 14%positive => (-1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 15%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i)+ 1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 16%positive => (-1 * (s IDsetparams_i)+ 1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 17%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i)+ 1 * (s IDsetparams_pset_dref_off24) <= 0 /\ 1 * (s IDsetparams__tmp) <= 0 /\ -1 * (s IDsetparams__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDsetparams__tmp) <= 0 /\ 1 * (s IDsetparams__tmp) <= 0 /\ -1 * (s IDsetparams_i)+ 1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 19%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 21%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 23%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 25%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ 1 * (s IDsetparams_code) <= 0 /\ -1 * (s IDsetparams_code) <= 0)%Z
    | 26%positive => (-1 * (s IDsetparams_code) <= 0 /\ 1 * (s IDsetparams_code) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0)%Z
    | 27%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 28%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 30%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_code) <= 0)%Z
    | 31%positive => (-1 * (s IDsetparams_code) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 32%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ 1 * (s IDsetparams_code) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDsetparams_code) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 34%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ 1 * (s IDsetparams_code) + 1 <= 0 /\ 1 * (s IDsetparams__tmp) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDsetparams__tmp) + 1 <= 0 /\ 1 * (s IDsetparams_code) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 36%positive => (-1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0)%Z
    | 37%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0)%Z
    | 38%positive => (-1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDsetparams_i) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0)%Z
    | 40%positive => (-1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDsetparams_i) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 43%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 44%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 46%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 48%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 50%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 52%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 54%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_code) <= 0)%Z
    | 55%positive => (-1 * (s IDsetparams_code) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 56%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ 1 * (s IDsetparams_code) + 1 <= 0)%Z
    | 57%positive => (1 * (s IDsetparams_code) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 58%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ 1 * (s IDsetparams_code) + 1 <= 0 /\ 1 * (s IDsetparams__tmp) + 1 <= 0)%Z
    | 59%positive => (1 * (s IDsetparams__tmp) + 1 <= 0 /\ 1 * (s IDsetparams_code) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 60%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 61%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ 1 * (s IDsetparams__tmp) + 15 <= 0 /\ -1 * (s IDsetparams__tmp) + -15 <= 0)%Z
    | 62%positive => (-1 * (s IDsetparams__tmp) + -15 <= 0 /\ 1 * (s IDsetparams__tmp) + 15 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 63%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 64%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 65%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 66%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 67%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 68%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0)%Z
    | 69%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0)%Z
    | 70%positive => (1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_i) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 71%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_i) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDsetparams_i) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0)%Z
    | 73%positive => (-1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_i) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDsetparams_i) + 1 <= 0 /\ 1 * (s IDsetparams_i)+ -1 * (s IDsetparams_pset_dref_off8) <= 0 /\ -1 * (s IDsetparams_pset_dref_off24) <= 0 /\ -1 * (s IDsetparams_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition setparams_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDsetparams_pset_dref_off24))
                     + max0((s IDsetparams_pset_dref_off8)))%Q
    | 2%positive => ((s IDsetparams_z)
                     + max0((s IDsetparams_pset_dref_off24))
                     + max0((s IDsetparams_pset_dref_off8)))%Q
    | 3%positive => ((s IDsetparams_z)
                     + max0((s IDsetparams_pset_dref_off24))
                     + max0((s IDsetparams_pset_dref_off8)))%Q
    | 4%positive => ((s IDsetparams_z)
                     + max0((s IDsetparams_pset_dref_off24))
                     + max0((s IDsetparams_pset_dref_off8)))%Q
    | 5%positive => ((s IDsetparams_z)
                     + max0((s IDsetparams_pset_dref_off24))
                     + max0((s IDsetparams_pset_dref_off8)))%Q
    | 6%positive => ((s IDsetparams_z)
                     + max0((s IDsetparams_pset_dref_off24))
                     + max0((s IDsetparams_pset_dref_off8)))%Q
    | 7%positive => ((s IDsetparams_z)
                     + max0(-(s IDsetparams_i)
                            + (s IDsetparams_pset_dref_off8))
                     + max0((s IDsetparams_pset_dref_off24)))%Q
    | 8%positive => ((s IDsetparams_z)
                     + max0(-(s IDsetparams_i)
                            + (s IDsetparams_pset_dref_off8))
                     + max0((s IDsetparams_pset_dref_off24)))%Q
    | 9%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                     + max0(-(s IDsetparams_i)
                            + (s IDsetparams_pset_dref_off8)))%Q
    | 10%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 11%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z))%Q
    | 12%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 13%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 14%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 15%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 16%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 17%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 18%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 19%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24))
                      - max0((s IDsetparams_pset_dref_off24)))%Q
    | 20%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 21%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 22%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 23%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 24%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 25%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 26%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 27%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 28%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 29%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 30%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 31%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 32%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 33%positive => ((1 # 1) + (s IDsetparams_z))%Q
    | 34%positive => ((1 # 1) + (s IDsetparams_z))%Q
    | 35%positive => ((1 # 1) + (s IDsetparams_z))%Q
    | 36%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 37%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 38%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 39%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 40%positive => ((1 # 1) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 41%positive => ((s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off24)))%Q
    | 42%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 43%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 44%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 45%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 46%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 47%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 48%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 49%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 50%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 51%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 52%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 53%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 54%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 55%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 56%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 57%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z))%Q
    | 58%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z))%Q
    | 59%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z))%Q
    | 60%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 61%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + (1 # 14) * max0(-1 - (s IDsetparams__tmp))
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 62%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + (1 # 14) * max0(-1 - (s IDsetparams__tmp))
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 63%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 64%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 65%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 66%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 67%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 68%positive => ((s IDsetparams_z))%Q
    | 69%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 70%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-1 - (s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 71%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 72%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 73%positive => ((1 # 1) + (s IDsetparams_pset_dref_off24)
                      + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | 74%positive => ((s IDsetparams_pset_dref_off24) + (s IDsetparams_z)
                      + max0(-(s IDsetparams_i)
                             + (s IDsetparams_pset_dref_off8)))%Q
    | _ => (0 # 1)%Q
  end.

Definition setparams_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsetparams_pset_dref_off24))) (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDsetparams_i)
                                                             + (s IDsetparams_pset_dref_off8)) (-1
                                                                    - (s IDsetparams_i)
                                                                    + (s IDsetparams_pset_dref_off8)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDsetparams_i)
                                            + (s IDsetparams_pset_dref_off8))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDsetparams_i)
                                                             + (s IDsetparams_pset_dref_off24)) (-1
                                                                    - (s IDsetparams_i)
                                                                    + (s IDsetparams_pset_dref_off24)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDsetparams_i)
                                            + (s IDsetparams_pset_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDsetparams_pset_dref_off24)))]
    | 19%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDsetparams_i)
                                                     + (s IDsetparams_pset_dref_off24)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDsetparams_pset_dref_off24)))]
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
    | 32%positive => [(*-1 0*) F_max0_ge_0 (-1 - (s IDsetparams_i)
                                            + (s IDsetparams_pset_dref_off24))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_one]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsetparams_pset_dref_off24))) (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))]
    | 42%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDsetparams_i)
                                                     + (s IDsetparams_pset_dref_off8)) (1)]
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
    | 56%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s IDsetparams_i)
                                                                + (s IDsetparams_pset_dref_off8))) (F_check_ge (0) (0))]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 ((s IDsetparams_pset_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDsetparams_pset_dref_off24)))]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*-1 0*) F_max0_ge_0 ((s IDsetparams_pset_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDsetparams_pset_dref_off24)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s IDsetparams_i)
                                                                + (s IDsetparams_pset_dref_off8))) (F_check_ge (0) (0));
                      (*-0.0714286 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDsetparams__tmp))) (F_check_ge (0) (0))]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDsetparams_i)
                                            + (s IDsetparams_pset_dref_off8));
                      (*-1 0*) F_max0_ge_0 ((s IDsetparams_pset_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsetparams_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDsetparams_pset_dref_off24)))]
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | _ => []
  end.


Theorem setparams_ai_correct:
  forall s p' s', steps (g_start setparams) s (g_edges setparams) p' s' -> setparams_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem setparams_pot_correct:
  forall s p' s',
    steps (g_start setparams) s (g_edges setparams) p' s' ->
    (setparams_pot (g_start setparams) s >= setparams_pot p' s')%Q.
Proof.
  check_lp setparams_ai_correct setparams_hints.
Qed.

