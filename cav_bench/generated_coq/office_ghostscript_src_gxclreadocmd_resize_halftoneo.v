Require Import pasta.Pasta.

Notation IDcmd_resize_halftone_z := 1%positive.
Notation IDcmd_resize_halftone__tmp := 2%positive.
Notation IDcmd_resize_halftone__tmp1 := 3%positive.
Notation IDcmd_resize_halftone_i := 4%positive.
Notation IDcmd_resize_halftone_pdht_dref_off136 := 5%positive.
Notation IDcmd_resize_halftone_mem := 6%positive.
Notation IDcmd_resize_halftone_num_comp := 7%positive.
Notation IDcmd_resize_halftone_pdht := 8%positive.
Definition cmd_resize_halftone : graph := {|
  g_start := 1%positive;
  g_end := 48%positive;
  g_edges := (1%positive,(AAssign IDcmd_resize_halftone_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone_pdht_dref_off136)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDcmd_resize_halftone__tmp
             (Some (EVar IDcmd_resize_halftone_num_comp))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) <>
             (eval (EVar IDcmd_resize_halftone_pdht_dref_off136) s))%Z)),
             10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) =
             (eval (EVar IDcmd_resize_halftone_pdht_dref_off136) s))%Z)),
             9%positive)::(9%positive,AWeaken,45%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) <
             (eval (EVar IDcmd_resize_halftone_pdht_dref_off136) s))%Z)),
             25%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) >=
             (eval (EVar IDcmd_resize_halftone_pdht_dref_off136) s))%Z)),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone_pdht_dref_off136)
             s) = (eval (ENum (0)) s))%Z)),17%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone_pdht_dref_off136)
             s) <> (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,20%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,22%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,43%positive)::
             (22%positive,(AAssign IDcmd_resize_halftone__tmp1
             (Some (ENum (-25)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,48%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDcmd_resize_halftone_i
             (Some (EVar IDcmd_resize_halftone_pdht_dref_off136))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDcmd_resize_halftone_i
             (Some (EAdd (EVar IDcmd_resize_halftone_i) (ENum (-1))))),
             29%positive)::(29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone_i) s) >
             (eval (EVar IDcmd_resize_halftone__tmp) s))%Z)),49%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone_i) s) <=
             (eval (EVar IDcmd_resize_halftone__tmp) s))%Z)),31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) =
             (eval (ENum (0)) s))%Z)),40%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_resize_halftone__tmp) s) <>
             (eval (ENum (0)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,36%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,42%positive)::
             (36%positive,(AAssign IDcmd_resize_halftone_pdht_dref_off136
             (Some (EVar IDcmd_resize_halftone__tmp))),37%positive)::
             (37%positive,(AAssign IDcmd_resize_halftone__tmp1
             (Some (ENum (-25)))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,48%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDcmd_resize_halftone_pdht_dref_off136
             (Some (EVar IDcmd_resize_halftone__tmp))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDcmd_resize_halftone__tmp1
             (Some (ENum (0)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,51%positive)::
             (50%positive,ANone,52%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDcmd_resize_halftone_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_resize_halftone_z)))),
             28%positive)::nil
|}.

Definition cmd_resize_halftone_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 4%positive => (-1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDcmd_resize_halftone__tmp) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 7%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 8%positive => (-1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 9%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 10%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 11%positive => (-1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 13%positive => (-1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 18%positive => (1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 19%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 20%positive => (-1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 21%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 22%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 23%positive => (-1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) + 25 <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp1) + -25 <= 0)%Z
    | 24%positive => (-1 * (s IDcmd_resize_halftone__tmp1) + -25 <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) + 25 <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 25%positive => (-1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 27%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 28%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 30%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 32%positive => (-1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 34%positive => (-1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 36%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 37%positive => (-1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_i)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 38%positive => (1 * (s IDcmd_resize_halftone_i)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_i)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) + 25 <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp1) + -25 <= 0)%Z
    | 39%positive => (-1 * (s IDcmd_resize_halftone__tmp1) + -25 <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) + 25 <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_i)+ 1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone_i)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 40%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp) <= 0)%Z
    | 41%positive => (-1 * (s IDcmd_resize_halftone__tmp) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp)+ 1 * (s IDcmd_resize_halftone_i) <= 0)%Z
    | 43%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0)%Z
    | 44%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0)%Z
    | 45%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0)%Z
    | 46%positive => (-1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) <= 0 /\ -1 * (s IDcmd_resize_halftone__tmp1) <= 0)%Z
    | 47%positive => (-1 * (s IDcmd_resize_halftone__tmp1) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0)%Z
    | 48%positive => (-1 * (s IDcmd_resize_halftone__tmp1) + -25 <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp1) <= 0)%Z
    | 49%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0)%Z
    | 50%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0)%Z
    | 52%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0)%Z
    | 54%positive => (1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_i) + 1 <= 0 /\ -1 * (s IDcmd_resize_halftone_z) <= 0 /\ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) <= 0 /\ 1 * (s IDcmd_resize_halftone__tmp)+ -1 * (s IDcmd_resize_halftone_pdht_dref_off136) + 1 <= 0)%Z
    | _ => False
  end.

Definition cmd_resize_halftone_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 - (s IDcmd_resize_halftone_num_comp)
                          + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 2%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone_num_comp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 3%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone_num_comp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 4%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone_num_comp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 5%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone_num_comp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 6%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone_num_comp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 7%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone__tmp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 8%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone__tmp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 9%positive => ((s IDcmd_resize_halftone_z)
                     + max0(-1 - (s IDcmd_resize_halftone__tmp)
                            + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 10%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 11%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 12%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 13%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 14%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 15%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 16%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 17%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 18%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 19%positive => (max0(-1 - (s IDcmd_resize_halftone__tmp)
                           + (s IDcmd_resize_halftone_pdht_dref_off136))
                      + max0((s IDcmd_resize_halftone_z)))%Q
    | 20%positive => ((s IDcmd_resize_halftone_z))%Q
    | 21%positive => ((s IDcmd_resize_halftone_z))%Q
    | 22%positive => ((s IDcmd_resize_halftone_z))%Q
    | 23%positive => ((s IDcmd_resize_halftone_z))%Q
    | 24%positive => ((s IDcmd_resize_halftone_z))%Q
    | 25%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 26%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 27%positive => ((s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 28%positive => ((s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 29%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 30%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 31%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 32%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-(s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_i)))%Q
    | 33%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-(s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_i)))%Q
    | 34%positive => ((s IDcmd_resize_halftone_z))%Q
    | 35%positive => ((s IDcmd_resize_halftone_z))%Q
    | 36%positive => ((s IDcmd_resize_halftone_z))%Q
    | 37%positive => ((s IDcmd_resize_halftone_z))%Q
    | 38%positive => ((s IDcmd_resize_halftone_z))%Q
    | 39%positive => ((s IDcmd_resize_halftone_z))%Q
    | 40%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-(s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_i)))%Q
    | 41%positive => ((s IDcmd_resize_halftone_z))%Q
    | 42%positive => ((s IDcmd_resize_halftone_z))%Q
    | 43%positive => ((s IDcmd_resize_halftone_z))%Q
    | 44%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 45%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 46%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 47%positive => ((s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 48%positive => ((s IDcmd_resize_halftone_z))%Q
    | 49%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 50%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 51%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 52%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 53%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | 54%positive => ((1 # 1) + (s IDcmd_resize_halftone_i)
                      - (s IDcmd_resize_halftone_pdht_dref_off136)
                      + (s IDcmd_resize_halftone_z)
                      + max0(-1 - (s IDcmd_resize_halftone__tmp)
                             + (s IDcmd_resize_halftone_pdht_dref_off136)))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_resize_halftone_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_resize_halftone_z)) (0))) (F_max0_ge_0 ((s IDcmd_resize_halftone_z)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_resize_halftone_z))) (F_check_ge ((s IDcmd_resize_halftone_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDcmd_resize_halftone__tmp)
                                                                 + (s IDcmd_resize_halftone_pdht_dref_off136))) (F_check_ge (0) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_resize_halftone_z))) (F_check_ge ((s IDcmd_resize_halftone_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDcmd_resize_halftone__tmp)
                                                                 + (s IDcmd_resize_halftone_pdht_dref_off136))) (F_check_ge (0) (0))]
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
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcmd_resize_halftone__tmp)
                                                                    + (s IDcmd_resize_halftone_i)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcmd_resize_halftone__tmp)
                                                                    + (s IDcmd_resize_halftone_i)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  - (s IDcmd_resize_halftone__tmp)
                                                                  + (s IDcmd_resize_halftone_pdht_dref_off136))) (F_check_ge (-1
                                                                    - (s IDcmd_resize_halftone__tmp)
                                                                    + (s IDcmd_resize_halftone_pdht_dref_off136)) (0))]
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcmd_resize_halftone__tmp)
                                                             + (s IDcmd_resize_halftone_i)) (-1
                                                                    - (s IDcmd_resize_halftone__tmp)
                                                                    + (s IDcmd_resize_halftone_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDcmd_resize_halftone__tmp)
                                            + (s IDcmd_resize_halftone_i))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcmd_resize_halftone__tmp)
                                                             + (s IDcmd_resize_halftone_i)) (-1
                                                                    - (s IDcmd_resize_halftone__tmp)
                                                                    + (s IDcmd_resize_halftone_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDcmd_resize_halftone__tmp)
                                            + (s IDcmd_resize_halftone_i))]
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDcmd_resize_halftone__tmp)
                                                                 + (s IDcmd_resize_halftone_pdht_dref_off136))) (F_check_ge (0) (0))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | _ => []
  end.


Theorem cmd_resize_halftone_ai_correct:
  forall s p' s', steps (g_start cmd_resize_halftone) s (g_edges cmd_resize_halftone) p' s' -> cmd_resize_halftone_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_resize_halftone_pot_correct:
  forall s p' s',
    steps (g_start cmd_resize_halftone) s (g_edges cmd_resize_halftone) p' s' ->
    (cmd_resize_halftone_pot (g_start cmd_resize_halftone) s >= cmd_resize_halftone_pot p' s')%Q.
Proof.
  check_lp cmd_resize_halftone_ai_correct cmd_resize_halftone_hints.
Qed.

