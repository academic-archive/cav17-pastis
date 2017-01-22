Require Import pasta.Pasta.

Notation IDcurrent_param_list_z := 1%positive.
Notation IDcurrent_param_list__tmp := 2%positive.
Notation IDcurrent_param_list_code := 3%positive.
Notation IDcurrent_param_list_code3 := 4%positive.
Notation IDcurrent_param_list_code6 := 5%positive.
Notation IDcurrent_param_list_i := 6%positive.
Notation IDcurrent_param_list_pset_dref_off24 := 7%positive.
Notation IDcurrent_param_list_pset_dref_off40 := 8%positive.
Notation IDcurrent_param_list_pset_dref_off8 := 9%positive.
Notation IDcurrent_param_list_op := 10%positive.
Notation IDcurrent_param_list_pset := 11%positive.
Notation IDcurrent_param_list_psref := 12%positive.
Definition current_param_list : graph := {|
  g_start := 1%positive;
  g_end := 82%positive;
  g_edges := (1%positive,(AAssign IDcurrent_param_list_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_pset_dref_off8)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_pset_dref_off40)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_pset_dref_off24)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) >=
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDcurrent_param_list_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) <
             (eval (EVar IDcurrent_param_list_pset_dref_off8) s))%Z)),
             63%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) >=
             (eval (EVar IDcurrent_param_list_pset_dref_off8) s))%Z)),
             11%positive)::(11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDcurrent_param_list_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) <
             (eval (EVar IDcurrent_param_list_pset_dref_off24) s))%Z)),
             44%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) >=
             (eval (EVar IDcurrent_param_list_pset_dref_off24) s))%Z)),
             16%positive)::(16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDcurrent_param_list_i (Some (ENum (0)))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) <
             (eval (EVar IDcurrent_param_list_pset_dref_off40) s))%Z)),
             25%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_i) s) >=
             (eval (EVar IDcurrent_param_list_pset_dref_off40) s))%Z)),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDcurrent_param_list__tmp
             (Some (ENum (0)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,82%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,29%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,34%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDcurrent_param_list_code6 None),
             30%positive)::(30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_code6) s) <
             (eval (ENum (0)) s))%Z)),40%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_code6) s) >=
             (eval (ENum (0)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDcurrent_param_list_i
             (Some (EAdd (EVar IDcurrent_param_list_i) (ENum (1))))),
             36%positive)::(36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDcurrent_param_list_z
             (Some (EAdd (ENum (1)) (EVar IDcurrent_param_list_z)))),
             39%positive)::(39%positive,AWeaken,20%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDcurrent_param_list__tmp
             (Some (EVar IDcurrent_param_list_code6))),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,82%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,48%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,53%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDcurrent_param_list_code3 None),
             49%positive)::(49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_code3) s) <
             (eval (ENum (0)) s))%Z)),59%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_code3) s) >=
             (eval (ENum (0)) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDcurrent_param_list_i
             (Some (EAdd (EVar IDcurrent_param_list_i) (ENum (1))))),
             55%positive)::(55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDcurrent_param_list_z
             (Some (EAdd (ENum (1)) (EVar IDcurrent_param_list_z)))),
             58%positive)::(58%positive,AWeaken,15%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AAssign IDcurrent_param_list__tmp
             (Some (EVar IDcurrent_param_list_code3))),61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,AWeaken,82%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,ANone,67%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,ANone,72%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDcurrent_param_list_code None),
             68%positive)::(68%positive,AWeaken,69%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_code) s) <
             (eval (ENum (0)) s))%Z)),78%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDcurrent_param_list_code) s) >=
             (eval (ENum (0)) s))%Z)),70%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDcurrent_param_list_i
             (Some (EAdd (EVar IDcurrent_param_list_i) (ENum (1))))),
             74%positive)::(74%positive,ANone,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,(AAssign IDcurrent_param_list_z
             (Some (EAdd (ENum (1)) (EVar IDcurrent_param_list_z)))),
             77%positive)::(77%positive,AWeaken,10%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AAssign IDcurrent_param_list__tmp
             (Some (EVar IDcurrent_param_list_code))),80%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,AWeaken,82%positive)::nil
|}.

Definition current_param_list_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0)%Z
    | 4%positive => (-1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 5%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 6%positive => (-1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 7%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 8%positive => (-1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 9%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 10%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0)%Z
    | 11%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off8) <= 0)%Z
    | 12%positive => (-1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0)%Z
    | 13%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 15%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 16%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 17%positive => (-1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 18%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 19%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 20%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 21%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 22%positive => (-1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 23%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list__tmp) <= 0 /\ -1 * (s IDcurrent_param_list__tmp) <= 0)%Z
    | 24%positive => (-1 * (s IDcurrent_param_list__tmp) <= 0 /\ 1 * (s IDcurrent_param_list__tmp) <= 0 /\ -1 * (s IDcurrent_param_list_i)+ 1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 25%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 27%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 29%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 31%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_code6) <= 0)%Z
    | 33%positive => (-1 * (s IDcurrent_param_list_code6) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0)%Z
    | 35%positive => (-1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 37%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0)%Z
    | 38%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 39%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) + 1 <= 0)%Z
    | 40%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_code6) + 1 <= 0)%Z
    | 41%positive => (1 * (s IDcurrent_param_list_code6) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ 1 * (s IDcurrent_param_list_code6) + 1 <= 0 /\ 1 * (s IDcurrent_param_list__tmp) + 1 <= 0)%Z
    | 43%positive => (1 * (s IDcurrent_param_list__tmp) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_code6) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off40) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 46%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 48%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 50%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_code3) <= 0)%Z
    | 52%positive => (-1 * (s IDcurrent_param_list_code3) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 54%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 56%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0)%Z
    | 57%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0)%Z
    | 58%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_z) + 1 <= 0)%Z
    | 59%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_code3) + 1 <= 0)%Z
    | 60%positive => (1 * (s IDcurrent_param_list_code3) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 61%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_code3) + 1 <= 0 /\ 1 * (s IDcurrent_param_list__tmp) + 1 <= 0)%Z
    | 62%positive => (1 * (s IDcurrent_param_list__tmp) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_code3) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off24) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 64%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 65%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 66%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 67%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 68%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 69%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 70%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_code) <= 0)%Z
    | 71%positive => (-1 * (s IDcurrent_param_list_code) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 72%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | 73%positive => (-1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0)%Z
    | 75%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0)%Z
    | 76%positive => (-1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0)%Z
    | 77%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) <= 0 /\ -1 * (s IDcurrent_param_list_i) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_z) + 1 <= 0)%Z
    | 78%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_code) + 1 <= 0)%Z
    | 79%positive => (1 * (s IDcurrent_param_list_code) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 80%positive => (1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ 1 * (s IDcurrent_param_list_code) + 1 <= 0 /\ 1 * (s IDcurrent_param_list__tmp) + 1 <= 0)%Z
    | 81%positive => (1 * (s IDcurrent_param_list__tmp) + 1 <= 0 /\ 1 * (s IDcurrent_param_list_code) + 1 <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off24) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ 1 * (s IDcurrent_param_list_i)+ -1 * (s IDcurrent_param_list_pset_dref_off8) + 1 <= 0)%Z
    | 82%positive => (1 * (s IDcurrent_param_list__tmp) <= 0 /\ -1 * (s IDcurrent_param_list_z) <= 0 /\ -1 * (s IDcurrent_param_list_i) <= 0 /\ -1 * (s IDcurrent_param_list_pset_dref_off40) <= 0)%Z
    | _ => False
  end.

Definition current_param_list_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 2%positive => ((s IDcurrent_param_list_z)
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 3%positive => ((s IDcurrent_param_list_z)
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 4%positive => ((s IDcurrent_param_list_z)
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 5%positive => ((s IDcurrent_param_list_z)
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 6%positive => ((s IDcurrent_param_list_z)
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 7%positive => ((s IDcurrent_param_list_z)
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40))
                     + max0((s IDcurrent_param_list_pset_dref_off8)))%Q
    | 8%positive => ((s IDcurrent_param_list_z)
                     + max0(-(s IDcurrent_param_list_i)
                            + (s IDcurrent_param_list_pset_dref_off8))
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40)))%Q
    | 9%positive => ((s IDcurrent_param_list_z)
                     + max0(-(s IDcurrent_param_list_i)
                            + (s IDcurrent_param_list_pset_dref_off8))
                     + max0((s IDcurrent_param_list_pset_dref_off24))
                     + max0((s IDcurrent_param_list_pset_dref_off40)))%Q
    | 10%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 11%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 12%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 13%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24))
                      - max0((s IDcurrent_param_list_pset_dref_off24)))%Q
    | 14%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24))
                      - max0((s IDcurrent_param_list_pset_dref_off24)))%Q
    | 15%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24))
                      - max0((s IDcurrent_param_list_pset_dref_off24)))%Q
    | 16%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24))
                      - max0((s IDcurrent_param_list_pset_dref_off24)))%Q
    | 17%positive => ((s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 18%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 19%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 20%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 21%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 22%positive => ((s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off40)))%Q
    | 23%positive => ((s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off40)))%Q
    | 24%positive => ((s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off40)))%Q
    | 25%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 26%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 27%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 28%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 29%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 30%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 31%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 32%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 33%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 34%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 35%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 36%positive => ((1 # 1) - (s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 37%positive => ((1 # 1) - (s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 38%positive => ((1 # 1) - (s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 39%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 40%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 41%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 42%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 43%positive => (-(s IDcurrent_param_list_i)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 44%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24))
                      - max0((s IDcurrent_param_list_pset_dref_off24)))%Q
    | 45%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 46%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 47%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 48%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 49%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 50%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 51%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 52%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 53%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 54%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 55%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 56%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 57%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 58%positive => ((s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 59%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off24)))%Q
    | 60%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 61%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 62%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 63%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 64%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 65%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 66%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 67%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 68%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 69%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 70%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 71%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 72%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 73%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 74%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 75%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 76%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 77%positive => ((s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-(s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 78%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z)
                      + max0(-1 - (s IDcurrent_param_list_i)
                             + (s IDcurrent_param_list_pset_dref_off8)))%Q
    | 79%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 80%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 81%positive => ((1 # 1) + (s IDcurrent_param_list_pset_dref_off24)
                      + (s IDcurrent_param_list_pset_dref_off40)
                      + (s IDcurrent_param_list_z))%Q
    | 82%positive => ((s IDcurrent_param_list_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition current_param_list_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcurrent_param_list_pset_dref_off40))) (F_check_ge ((s IDcurrent_param_list_pset_dref_off40)) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcurrent_param_list_pset_dref_off24))) (F_check_ge ((s IDcurrent_param_list_pset_dref_off24)) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcurrent_param_list_i)
                                                             + (s IDcurrent_param_list_pset_dref_off8)) (-1
                                                                    - (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off8)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcurrent_param_list_i)
                                            + (s IDcurrent_param_list_pset_dref_off8))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcurrent_param_list_i)
                                                             + (s IDcurrent_param_list_pset_dref_off24)) (-1
                                                                    - (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off24)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcurrent_param_list_i)
                                            + (s IDcurrent_param_list_pset_dref_off24));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcurrent_param_list_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off24)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off40)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off40)))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcurrent_param_list_i)
                                                             + (s IDcurrent_param_list_pset_dref_off40)) (-1
                                                                    - (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off40)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcurrent_param_list_i)
                                            + (s IDcurrent_param_list_pset_dref_off40))]
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
    | 43%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement (-(s IDcurrent_param_list_i)
                                                     + (s IDcurrent_param_list_pset_dref_off40)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcurrent_param_list_i)
                                            + (s IDcurrent_param_list_pset_dref_off40));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off40)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcurrent_param_list_i)
                                                                    + (s IDcurrent_param_list_pset_dref_off40)))]
    | 44%positive => [(*0 1*) F_max0_pre_decrement (-(s IDcurrent_param_list_i)
                                                    + (s IDcurrent_param_list_pset_dref_off24)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcurrent_param_list_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off24)))]
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
    | 58%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcurrent_param_list_pset_dref_off24))) (F_check_ge ((s IDcurrent_param_list_pset_dref_off24)) (0))]
    | 59%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s IDcurrent_param_list_i)
                                                                + (s IDcurrent_param_list_pset_dref_off24))) (F_check_ge (0) (0))]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off40));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcurrent_param_list_pset_dref_off40)) (0))) (F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off40)))]
    | 63%positive => [(*-8e-12 1*) F_max0_pre_decrement (-(s IDcurrent_param_list_i)
                                                         + (s IDcurrent_param_list_pset_dref_off8)) (1)]
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
    | 77%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcurrent_param_list_z))) (F_check_ge ((s IDcurrent_param_list_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcurrent_param_list_z)) (0))) (F_max0_ge_0 ((s IDcurrent_param_list_z)))]
    | 78%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                - (s IDcurrent_param_list_i)
                                                                + (s IDcurrent_param_list_pset_dref_off8))) (F_check_ge (0) (0))]
    | 79%positive => []
    | 80%positive => []
    | 81%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off24));
                      (*-1 0*) F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off40));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcurrent_param_list_pset_dref_off40)) (0))) (F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off40)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcurrent_param_list_pset_dref_off24)) (0))) (F_max0_ge_0 ((s IDcurrent_param_list_pset_dref_off24)))]
    | 82%positive => []
    | _ => []
  end.


Theorem current_param_list_ai_correct:
  forall s p' s', steps (g_start current_param_list) s (g_edges current_param_list) p' s' -> current_param_list_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem current_param_list_pot_correct:
  forall s p' s',
    steps (g_start current_param_list) s (g_edges current_param_list) p' s' ->
    (current_param_list_pot (g_start current_param_list) s >= current_param_list_pot p' s')%Q.
Proof.
  check_lp current_param_list_ai_correct current_param_list_hints.
Qed.

