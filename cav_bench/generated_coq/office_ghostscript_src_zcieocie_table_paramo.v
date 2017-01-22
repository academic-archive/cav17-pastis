Require Import pasta.Pasta.

Notation IDcie_table_param_z := 1%positive.
Notation IDcie_table_param__tmp := 2%positive.
Notation IDcie_table_param_code := 3%positive.
Notation IDcie_table_param_d0 := 4%positive.
Notation IDcie_table_param_d1 := 5%positive.
Notation IDcie_table_param_i := 6%positive.
Notation IDcie_table_param_m := 7%positive.
Notation IDcie_table_param_n := 8%positive.
Notation IDcie_table_param_nbytes := 9%positive.
Notation IDcie_table_param_ntables := 10%positive.
Notation IDcie_table_param_pclt_dref_off0 := 11%positive.
Notation IDcie_table_param_pclt_dref_off20 := 12%positive.
Notation IDcie_table_param_pclt_dref_off4_off0 := 13%positive.
Notation IDcie_table_param_pclt_dref_off4_off4 := 14%positive.
Notation IDcie_table_param_mem := 15%positive.
Notation IDcie_table_param_pclt := 16%positive.
Notation IDcie_table_param_ptref := 17%positive.
Definition cie_table_param : graph := {|
  g_start := 1%positive;
  g_end := 91%positive;
  g_edges := (1%positive,(AAssign IDcie_table_param_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcie_table_param_n
             (Some (EVar IDcie_table_param_pclt_dref_off0))),3%positive)::
             (3%positive,(AAssign IDcie_table_param_m
             (Some (EVar IDcie_table_param_pclt_dref_off20))),4%positive)::
             (4%positive,(AAssign IDcie_table_param_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDcie_table_param_i)
             s) < (eval (EVar IDcie_table_param_n) s))%Z)),73%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDcie_table_param_i)
             s) >= (eval (EVar IDcie_table_param_n) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDcie_table_param_nbytes None),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDcie_table_param_n)
             s) = (eval (ENum (3)) s))%Z)),56%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDcie_table_param_n)
             s) <> (eval (ENum (3)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDcie_table_param_d0
             (Some (EVar IDcie_table_param_pclt_dref_off4_off0))),
             14%positive)::
             (14%positive,(AAssign IDcie_table_param_d1
             (Some (EVar IDcie_table_param_pclt_dref_off4_off4))),
             15%positive)::
             (15%positive,(AAssign IDcie_table_param_ntables
             (Some (EMul (EVar IDcie_table_param_d0)
             (EVar IDcie_table_param_d1)))),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,26%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,24%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDcie_table_param__tmp None),22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,91%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,91%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,53%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,50%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDcie_table_param_i (Some (ENum (0)))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDcie_table_param_i)
             s) < (eval (EVar IDcie_table_param_d0) s))%Z)),35%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDcie_table_param_i)
             s) >= (eval (EVar IDcie_table_param_d0) s))%Z)),34%positive)::
             (34%positive,AWeaken,48%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDcie_table_param_code None),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDcie_table_param_code) s) <
             (eval (ENum (0)) s))%Z)),46%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDcie_table_param_code) s) >=
             (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDcie_table_param_i
             (Some (EAdd (EVar IDcie_table_param_i) (ENum (1))))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDcie_table_param_z (Some (EAdd (ENum (1))
             (EVar IDcie_table_param_z)))),45%positive)::
             (45%positive,AWeaken,33%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,61%positive)::
             (50%positive,(AAssign IDcie_table_param__tmp
             (Some (ENum (-25)))),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,91%positive)::
             (53%positive,(AAssign IDcie_table_param__tmp
             (Some (ENum (-15)))),54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,91%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,70%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDcie_table_param_code None),59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AGuard
             (fun s => ((eval (EVar IDcie_table_param_code) s) <
             (eval (ENum (0)) s))%Z)),66%positive)::
             (61%positive,(AGuard
             (fun s => ((eval (EVar IDcie_table_param_code) s) >=
             (eval (ENum (0)) s))%Z)),62%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AAssign IDcie_table_param__tmp (Some (ENum (0)))),
             64%positive)::(64%positive,ANone,65%positive)::
             (65%positive,AWeaken,91%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,(AAssign IDcie_table_param__tmp
             (Some (EVar IDcie_table_param_code))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,AWeaken,91%positive)::
             (70%positive,(AAssign IDcie_table_param__tmp
             (Some (ENum (-25)))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,91%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,ANone,78%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDcie_table_param__tmp
             (Some (ENum (-20)))),76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,AWeaken,91%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,ANone,88%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,ANone,88%positive)::
             (81%positive,ANone,82%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,(AAssign IDcie_table_param_i
             (Some (EAdd (EVar IDcie_table_param_i) (ENum (1))))),
             84%positive)::(84%positive,ANone,85%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,(AAssign IDcie_table_param_z (Some (EAdd (ENum (1))
             (EVar IDcie_table_param_z)))),87%positive)::
             (87%positive,AWeaken,7%positive)::
             (88%positive,(AAssign IDcie_table_param__tmp
             (Some (ENum (-15)))),89%positive)::
             (89%positive,ANone,90%positive)::
             (90%positive,AWeaken,91%positive)::nil
|}.

Definition cie_table_param_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_z) <= 0)%Z
    | 4%positive => (1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 8%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 9%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 13%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 15%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 17%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 18%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 19%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 20%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 21%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 22%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 23%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 24%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 25%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 26%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 27%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 28%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 29%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 30%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 31%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 32%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 33%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 34%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_d0)+ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 35%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 37%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 39%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_code) <= 0)%Z
    | 40%positive => (-1 * (s IDcie_table_param_code) <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 41%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_code) <= 0)%Z
    | 42%positive => (-1 * (s IDcie_table_param_code) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) <= 0)%Z
    | 43%positive => (-1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_code) <= 0)%Z
    | 44%positive => (-1 * (s IDcie_table_param_code) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) <= 0)%Z
    | 45%positive => (-1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_code) <= 0 /\ -1 * (s IDcie_table_param_z) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ 1 * (s IDcie_table_param_code) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDcie_table_param_code) + 1 <= 0 /\ -1 * (s IDcie_table_param_d0)+ 1 * (s IDcie_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 48%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 49%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 50%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 51%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param__tmp) + 25 <= 0 /\ -1 * (s IDcie_table_param__tmp) + -25 <= 0)%Z
    | 52%positive => (-1 * (s IDcie_table_param__tmp) + -25 <= 0 /\ 1 * (s IDcie_table_param__tmp) + 25 <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 53%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 54%positive => (-1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param__tmp) + 15 <= 0 /\ -1 * (s IDcie_table_param__tmp) + -15 <= 0)%Z
    | 55%positive => (-1 * (s IDcie_table_param__tmp) + -15 <= 0 /\ 1 * (s IDcie_table_param__tmp) + 15 <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0)%Z
    | 56%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_n) + 3 <= 0)%Z
    | 57%positive => (-1 * (s IDcie_table_param_n) + 3 <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 58%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_n) + 3 <= 0)%Z
    | 59%positive => (-1 * (s IDcie_table_param_n) + 3 <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 60%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_n) + 3 <= 0)%Z
    | 61%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 62%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_code) <= 0)%Z
    | 63%positive => (-1 * (s IDcie_table_param_code) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 64%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_code) <= 0 /\ 1 * (s IDcie_table_param__tmp) <= 0 /\ -1 * (s IDcie_table_param__tmp) <= 0)%Z
    | 65%positive => (-1 * (s IDcie_table_param__tmp) <= 0 /\ 1 * (s IDcie_table_param__tmp) <= 0 /\ -1 * (s IDcie_table_param_code) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 66%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param_code) + 1 <= 0)%Z
    | 67%positive => (1 * (s IDcie_table_param_code) + 1 <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 68%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param_code) + 1 <= 0 /\ 1 * (s IDcie_table_param__tmp) + 1 <= 0)%Z
    | 69%positive => (1 * (s IDcie_table_param__tmp) + 1 <= 0 /\ 1 * (s IDcie_table_param_code) + 1 <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 70%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_n) + 3 <= 0)%Z
    | 71%positive => (-1 * (s IDcie_table_param_n) + 3 <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param__tmp) + 25 <= 0 /\ -1 * (s IDcie_table_param__tmp) + -25 <= 0)%Z
    | 72%positive => (-1 * (s IDcie_table_param__tmp) + -25 <= 0 /\ 1 * (s IDcie_table_param__tmp) + 25 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i)+ 1 * (s IDcie_table_param_n) <= 0 /\ 1 * (s IDcie_table_param_n) + -3 <= 0 /\ -1 * (s IDcie_table_param_n) + 3 <= 0)%Z
    | 73%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 74%positive => (1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 75%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 76%positive => (1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param__tmp) + 20 <= 0 /\ -1 * (s IDcie_table_param__tmp) + -20 <= 0)%Z
    | 77%positive => (-1 * (s IDcie_table_param__tmp) + -20 <= 0 /\ 1 * (s IDcie_table_param__tmp) + 20 <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 78%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 79%positive => (1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 80%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 81%positive => (1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 82%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 83%positive => (1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | 84%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_i) + 1 <= 0)%Z
    | 85%positive => (-1 * (s IDcie_table_param_i) + 1 <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0)%Z
    | 86%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_i) + 1 <= 0)%Z
    | 87%positive => (-1 * (s IDcie_table_param_i) + 1 <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) <= 0 /\ -1 * (s IDcie_table_param_z) + 1 <= 0)%Z
    | 88%positive => (-1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 89%positive => (1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ 1 * (s IDcie_table_param__tmp) + 15 <= 0 /\ -1 * (s IDcie_table_param__tmp) + -15 <= 0)%Z
    | 90%positive => (-1 * (s IDcie_table_param__tmp) + -15 <= 0 /\ 1 * (s IDcie_table_param__tmp) + 15 <= 0 /\ -1 * (s IDcie_table_param_i) <= 0 /\ -1 * (s IDcie_table_param_z) <= 0 /\ 1 * (s IDcie_table_param_i)+ -1 * (s IDcie_table_param_n) + 1 <= 0)%Z
    | 91%positive => (-1 * (s IDcie_table_param_z) <= 0 /\ -1 * (s IDcie_table_param_i) <= 0)%Z
    | _ => False
  end.

Definition cie_table_param_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcie_table_param_pclt_dref_off0))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 2%positive => ((s IDcie_table_param_z)
                     + max0((s IDcie_table_param_pclt_dref_off0))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 3%positive => ((s IDcie_table_param_z) + max0((s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 4%positive => ((s IDcie_table_param_z) + max0((s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 5%positive => ((s IDcie_table_param_z)
                     + max0(-(s IDcie_table_param_i)
                            + (s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 6%positive => ((s IDcie_table_param_z)
                     + max0(-(s IDcie_table_param_i)
                            + (s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 7%positive => ((s IDcie_table_param_z)
                     + max0(-(s IDcie_table_param_i)
                            + (s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 8%positive => ((s IDcie_table_param_z)
                     + max0(-(s IDcie_table_param_i)
                            + (s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 9%positive => ((s IDcie_table_param_z)
                     + max0(-(s IDcie_table_param_i)
                            + (s IDcie_table_param_n))
                     + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 10%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 11%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 12%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 13%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 14%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 15%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 16%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 17%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 18%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 19%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 20%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 21%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 22%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 23%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 24%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 25%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 26%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 27%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 28%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 29%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)))%Q
    | 30%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)))%Q
    | 31%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 32%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 33%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 34%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 35%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 36%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 37%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 38%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 39%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 40%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 41%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 42%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 43%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 44%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 45%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 46%positive => ((1 # 1) + (s IDcie_table_param_z)
                      + max0(-1 + (s IDcie_table_param_d0)
                             - (s IDcie_table_param_i)))%Q
    | 47%positive => ((s IDcie_table_param_z))%Q
    | 48%positive => ((s IDcie_table_param_z))%Q
    | 49%positive => ((s IDcie_table_param_z))%Q
    | 50%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)))%Q
    | 51%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)))%Q
    | 52%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0)))%Q
    | 53%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 54%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 55%positive => ((s IDcie_table_param_z)
                      + max0((s IDcie_table_param_d0))
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n)))%Q
    | 56%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 57%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 58%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 59%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 60%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 61%positive => ((s IDcie_table_param_z))%Q
    | 62%positive => ((s IDcie_table_param_z))%Q
    | 63%positive => ((s IDcie_table_param_z))%Q
    | 64%positive => ((s IDcie_table_param_z))%Q
    | 65%positive => ((s IDcie_table_param_z))%Q
    | 66%positive => ((s IDcie_table_param_z))%Q
    | 67%positive => ((s IDcie_table_param_z))%Q
    | 68%positive => ((s IDcie_table_param_z))%Q
    | 69%positive => ((s IDcie_table_param_z))%Q
    | 70%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 71%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 72%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 73%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 74%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 75%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 76%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 77%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 78%positive => ((s IDcie_table_param_z)
                      + max0(-(s IDcie_table_param_i)
                             + (s IDcie_table_param_n))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 79%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 80%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 81%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 82%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 83%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 84%positive => ((1 # 1) - (s IDcie_table_param_i)
                      + (s IDcie_table_param_n) + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 85%positive => ((1 # 1) - (s IDcie_table_param_i)
                      + (s IDcie_table_param_n) + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 86%positive => ((1 # 1) - (s IDcie_table_param_i)
                      + (s IDcie_table_param_n) + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 87%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 88%positive => (-(s IDcie_table_param_i) + (s IDcie_table_param_n)
                      + (s IDcie_table_param_z)
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 89%positive => (-(1 # 1) - (s IDcie_table_param_i)
                      + (s IDcie_table_param_n) + (s IDcie_table_param_z)
                      + (1 # 5) * max0(20 + (s IDcie_table_param__tmp))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 90%positive => (-(1 # 1) - (s IDcie_table_param_i)
                      + (s IDcie_table_param_n) + (s IDcie_table_param_z)
                      + (1 # 5) * max0(20 + (s IDcie_table_param__tmp))
                      + max0((s IDcie_table_param_pclt_dref_off4_off0)))%Q
    | 91%positive => ((s IDcie_table_param_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_table_param_hints (p : node) (s : state) := 
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
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_d0))]
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_d0))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDcie_table_param_d0)
                                                            - (s IDcie_table_param_i)) (-1
                                                                    + (s IDcie_table_param_d0)
                                                                    - (s IDcie_table_param_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcie_table_param_d0)
                                            - (s IDcie_table_param_i))]
    | 35%positive => [(*0 1*) F_max0_pre_decrement ((s IDcie_table_param_d0)
                                                    - (s IDcie_table_param_i)) (1)]
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
    | 46%positive => [(*0 1*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDcie_table_param_d0)
                                                                 - (s IDcie_table_param_i))) (F_check_ge (0) (0))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*-1 0*) F_max0_ge_0 ((s IDcie_table_param_d0))]
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_d0))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_pclt_dref_off4_off0))]
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
    | 72%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_pclt_dref_off4_off0))]
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcie_table_param_i)
                                                             + (s IDcie_table_param_n)) (-1
                                                                    - (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_pclt_dref_off4_off0))]
    | 78%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-(s IDcie_table_param_i)
                                                                   + 
                                                                   (s IDcie_table_param_n))) (F_check_ge (-
                                                                    (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)) (0))]
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)))]
    | 88%positive => []
    | 89%positive => []
    | 90%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDcie_table_param_i)
                                                     + (s IDcie_table_param_n)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcie_table_param_i)
                                            + (s IDcie_table_param_n));
                      (*-1 0*) F_max0_ge_0 ((s IDcie_table_param_pclt_dref_off4_off0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcie_table_param_i)
                                                                    + (s IDcie_table_param_n)));
                      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (20
                                                                   + 
                                                                   (s IDcie_table_param__tmp))) (F_check_ge (0) (0))]
    | 91%positive => []
    | _ => []
  end.


Theorem cie_table_param_ai_correct:
  forall s p' s', steps (g_start cie_table_param) s (g_edges cie_table_param) p' s' -> cie_table_param_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_table_param_pot_correct:
  forall s p' s',
    steps (g_start cie_table_param) s (g_edges cie_table_param) p' s' ->
    (cie_table_param_pot (g_start cie_table_param) s >= cie_table_param_pot p' s')%Q.
Proof.
  check_lp cie_table_param_ai_correct cie_table_param_hints.
Qed.

