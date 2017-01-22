Require Import pasta.Pasta.

Notation IDdict_check_uid_param_z := 1%positive.
Notation IDdict_check_uid_param__tmp := 2%positive.
Notation IDdict_check_uid_param_i := 3%positive.
Notation IDdict_check_uid_param_puid_dref_off0 := 4%positive.
Notation IDdict_check_uid_param_size := 5%positive.
Notation IDdict_check_uid_param_pdict := 6%positive.
Notation IDdict_check_uid_param_puid := 7%positive.
Definition dict_check_uid_param : graph := {|
  g_start := 1%positive;
  g_end := 55%positive;
  g_edges := (1%positive,(AAssign IDdict_check_uid_param_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDdict_check_uid_param_size) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDdict_check_uid_param_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDdict_check_uid_param_puid_dref_off0)
             s) < (eval (ENum (0)) s))%Z)),17%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDdict_check_uid_param_puid_dref_off0)
             s) >= (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,14%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::(9%positive,ANone,11%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDdict_check_uid_param__tmp None),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,55%positive)::
             (14%positive,(AAssign IDdict_check_uid_param__tmp
             (Some (ENum (0)))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,55%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDdict_check_uid_param_size
             (Some (ESub (ENum (0))
             (EVar IDdict_check_uid_param_puid_dref_off0)))),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,52%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (22%positive,ANone,49%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,49%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDdict_check_uid_param_i
             (Some (ENum (0)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDdict_check_uid_param_i) s) <
             (eval (EVar IDdict_check_uid_param_size) s))%Z)),33%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDdict_check_uid_param_i) s) >=
             (eval (EVar IDdict_check_uid_param_size) s))%Z)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDdict_check_uid_param__tmp
             (Some (ENum (1)))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,55%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,38%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDdict_check_uid_param__tmp
             (Some (ENum (0)))),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,55%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,46%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDdict_check_uid_param_i
             (Some (EAdd (EVar IDdict_check_uid_param_i) (ENum (1))))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDdict_check_uid_param_z
             (Some (EAdd (ENum (1)) (EVar IDdict_check_uid_param_z)))),
             45%positive)::(45%positive,AWeaken,28%positive)::
             (46%positive,(AAssign IDdict_check_uid_param__tmp
             (Some (ENum (0)))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,55%positive)::
             (49%positive,(AAssign IDdict_check_uid_param__tmp
             (Some (ENum (0)))),50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,55%positive)::
             (52%positive,(AAssign IDdict_check_uid_param__tmp
             (Some (ENum (0)))),53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,55%positive)::nil
|}.

Definition dict_check_uid_param_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 4%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0)%Z
    | 5%positive => (-1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 6%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0)%Z
    | 7%positive => (-1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 8%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0)%Z
    | 9%positive => (-1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 10%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0)%Z
    | 11%positive => (-1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 12%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0)%Z
    | 13%positive => (-1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 14%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0)%Z
    | 15%positive => (-1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ -1 * (s IDdict_check_uid_param__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_puid_dref_off0) <= 0)%Z
    | 17%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 19%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0)%Z
    | 21%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0)%Z
    | 23%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0)%Z
    | 25%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0)%Z
    | 27%positive => (-1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 29%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i)+ 1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 30%positive => (-1 * (s IDdict_check_uid_param_i)+ 1 * (s IDdict_check_uid_param_size) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 31%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i)+ 1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) + -1 <= 0 /\ -1 * (s IDdict_check_uid_param__tmp) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDdict_check_uid_param__tmp) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) + -1 <= 0 /\ -1 * (s IDdict_check_uid_param_i)+ 1 * (s IDdict_check_uid_param_size) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0)%Z
    | 33%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ -1 * (s IDdict_check_uid_param__tmp) <= 0)%Z
    | 37%positive => (-1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 39%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0)%Z
    | 40%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 41%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0 /\ -1 * (s IDdict_check_uid_param_i) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDdict_check_uid_param_i) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0)%Z
    | 44%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0 /\ -1 * (s IDdict_check_uid_param_i) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDdict_check_uid_param_i) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) + 1 <= 0)%Z
    | 46%positive => (1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ -1 * (s IDdict_check_uid_param__tmp) <= 0)%Z
    | 48%positive => (-1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_i)+ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ -1 * (s IDdict_check_uid_param__tmp) <= 0)%Z
    | 51%positive => (-1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 52%positive => (1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDdict_check_uid_param_size) + 1 <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ -1 * (s IDdict_check_uid_param__tmp) <= 0)%Z
    | 54%positive => (-1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param__tmp) <= 0 /\ 1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ 1 * (s IDdict_check_uid_param_puid_dref_off0) + 1 <= 0 /\ -1 * (s IDdict_check_uid_param_size) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDdict_check_uid_param_size) <= 0 /\ -1 * (s IDdict_check_uid_param_i) <= 0 /\ -1 * (s IDdict_check_uid_param_z) <= 0)%Z
    | _ => False
  end.

Definition dict_check_uid_param_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 2%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0))
                     + max0((s IDdict_check_uid_param_z)))%Q
    | 3%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0))
                     + max0((s IDdict_check_uid_param_z)))%Q
    | 4%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0))
                     + max0((s IDdict_check_uid_param_z)))%Q
    | 5%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0))
                     + max0((s IDdict_check_uid_param_z)))%Q
    | 6%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0))
                     + max0((s IDdict_check_uid_param_z)))%Q
    | 7%positive => ((s IDdict_check_uid_param_z)
                     + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 8%positive => ((s IDdict_check_uid_param_z)
                     + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 9%positive => ((s IDdict_check_uid_param_z)
                     + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 10%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 11%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 12%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 13%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 14%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 15%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 16%positive => ((s IDdict_check_uid_param_z)
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 17%positive => (max0(-(s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_z)))%Q
    | 18%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0(-(s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_z)))%Q
    | 19%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_size))
                      + max0((s IDdict_check_uid_param_z)))%Q
    | 20%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_size)))%Q
    | 21%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_size)))%Q
    | 22%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 23%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 24%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 25%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 26%positive => ((1 # 1) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 27%positive => ((1 # 1) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 28%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 29%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 30%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 31%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 32%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 33%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 34%positive => ((1 # 2) + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 35%positive => ((1 # 2) + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 36%positive => (-(1 # 2) + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      + max0(1 - (s IDdict_check_uid_param__tmp))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 37%positive => (-(1 # 2) + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      + max0(1 - (s IDdict_check_uid_param__tmp))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 38%positive => ((1 # 2) + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size))
                      - (1 # 2) * max0((s IDdict_check_uid_param_size)))%Q
    | 39%positive => ((1 # 2) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 40%positive => ((1 # 2) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 41%positive => ((1 # 2) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 42%positive => ((1 # 1) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 43%positive => ((1 # 1) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 44%positive => ((1 # 1) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 45%positive => (-(1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-(s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 46%positive => ((1 # 2) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 47%positive => ((1 # 2) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 48%positive => ((1 # 2) - (1 # 2) * (s IDdict_check_uid_param_i)
                      + (1 # 2) * (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + (1 # 2) * max0(-1 - (s IDdict_check_uid_param_i)
                                       + (s IDdict_check_uid_param_size)))%Q
    | 49%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 50%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 51%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_size)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0)))%Q
    | 52%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_size)))%Q
    | 53%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_size)))%Q
    | 54%positive => ((1 # 1) + (s IDdict_check_uid_param_puid_dref_off0)
                      + (s IDdict_check_uid_param_z)
                      + max0(-1 - (s IDdict_check_uid_param_puid_dref_off0))
                      + max0((s IDdict_check_uid_param_size)))%Q
    | 55%positive => ((s IDdict_check_uid_param_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition dict_check_uid_param_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDdict_check_uid_param_z))) (F_check_ge ((s IDdict_check_uid_param_z)) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_ge_0 (-(s IDdict_check_uid_param_puid_dref_off0))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_ge_0 (-(s IDdict_check_uid_param_puid_dref_off0))]
    | 17%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDdict_check_uid_param_puid_dref_off0)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDdict_check_uid_param_puid_dref_off0)))]
    | 18%positive => []
    | 19%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDdict_check_uid_param_z))) (F_check_ge ((s IDdict_check_uid_param_z)) (0))]
    | 20%positive => []
    | 21%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDdict_check_uid_param_size))) (F_check_ge ((s IDdict_check_uid_param_size)) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   - 
                                                                   (s IDdict_check_uid_param_puid_dref_off0))) (F_check_ge (-1
                                                                    - (s IDdict_check_uid_param_puid_dref_off0)) (0))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDdict_check_uid_param_i)
                                                             + (s IDdict_check_uid_param_size)) (-1
                                                                    - (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDdict_check_uid_param_i)
                                            + (s IDdict_check_uid_param_size));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 ((s IDdict_check_uid_param_size)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 (-
                                                                    (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)))]
    | 33%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-0.5 0*) F_max0_pre_decrement (-(s IDdict_check_uid_param_i)
                                                       + (s IDdict_check_uid_param_size)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDdict_check_uid_param_i)
                                            + (s IDdict_check_uid_param_size));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 ((s IDdict_check_uid_param_size)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDdict_check_uid_param__tmp))) (F_check_ge (0) (0))]
    | 38%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 ((s IDdict_check_uid_param_size)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDdict_check_uid_param_i)
                                                                    + 
                                                                    (s IDdict_check_uid_param_size))) (F_check_ge (-
                                                                    (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)) (0))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDdict_check_uid_param_size))) (F_check_ge ((s IDdict_check_uid_param_size)) (0))]
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDdict_check_uid_param_i)
                                                             + (s IDdict_check_uid_param_size)) (-1
                                                                    - (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDdict_check_uid_param_i)
                                            + (s IDdict_check_uid_param_size));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 (-
                                                                    (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    (s IDdict_check_uid_param_i)
                                                                    + 
                                                                    (s IDdict_check_uid_param_size))) (F_check_ge (-1
                                                                    - (s IDdict_check_uid_param_i)
                                                                    + (s IDdict_check_uid_param_size)) (0))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_max0_ge_0 ((s IDdict_check_uid_param_size));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdict_check_uid_param_size)) (0))) (F_max0_ge_0 ((s IDdict_check_uid_param_size)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   - 
                                                                   (s IDdict_check_uid_param_puid_dref_off0))) (F_check_ge (-1
                                                                    - (s IDdict_check_uid_param_puid_dref_off0)) (0))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => [(*-1 0*) F_max0_ge_0 ((s IDdict_check_uid_param_size));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   - 
                                                                   (s IDdict_check_uid_param_puid_dref_off0))) (F_check_ge (-1
                                                                    - (s IDdict_check_uid_param_puid_dref_off0)) (0))]
    | 55%positive => []
    | _ => []
  end.


Theorem dict_check_uid_param_ai_correct:
  forall s p' s', steps (g_start dict_check_uid_param) s (g_edges dict_check_uid_param) p' s' -> dict_check_uid_param_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem dict_check_uid_param_pot_correct:
  forall s p' s',
    steps (g_start dict_check_uid_param) s (g_edges dict_check_uid_param) p' s' ->
    (dict_check_uid_param_pot (g_start dict_check_uid_param) s >= dict_check_uid_param_pot p' s')%Q.
Proof.
  check_lp dict_check_uid_param_ai_correct dict_check_uid_param_hints.
Qed.

