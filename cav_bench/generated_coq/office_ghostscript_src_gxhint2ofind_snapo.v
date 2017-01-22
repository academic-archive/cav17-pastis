Require Import pasta.Pasta.

Notation IDfind_snap_z := 1%positive.
Notation IDfind_snap__tmp := 2%positive.
Notation IDfind_snap_adj_dv := 3%positive.
Notation IDfind_snap_best := 4%positive.
Notation IDfind_snap_diff := 5%positive.
Notation IDfind_snap_i := 6%positive.
Notation IDfind_snap_pps_dref_off0 := 7%positive.
Notation IDfind_snap_psst_dref_off0 := 8%positive.
Notation IDfind_snap_dv := 9%positive.
Notation IDfind_snap_pps := 10%positive.
Notation IDfind_snap_psst := 11%positive.
Definition find_snap : graph := {|
  g_start := 1%positive;
  g_end := 27%positive;
  g_edges := (1%positive,(AAssign IDfind_snap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfind_snap__tmp
             (Some (EVar IDfind_snap_dv))),3%positive)::
             (3%positive,(AAssign IDfind_snap_best
             (Some (EVar IDfind_snap_pps_dref_off0))),4%positive)::
             (4%positive,(AAssign IDfind_snap_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_i) s) <
             (eval (EVar IDfind_snap_psst_dref_off0) s))%Z)),28%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_i) s) >=
             (eval (EVar IDfind_snap_psst_dref_off0) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_best)
             s) < (eval (ENum (0)) s))%Z)),13%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_best)
             s) >= (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,16%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,18%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,19%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDfind_snap_adj_dv None),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_adj_dv)
             s) = (eval (ENum (0)) s))%Z)),23%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_adj_dv)
             s) <> (eval (ENum (0)) s))%Z)),22%positive)::
             (22%positive,AWeaken,27%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDfind_snap_adj_dv
             (Some (EVar IDfind_snap_pps_dref_off0))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDfind_snap_diff None),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_diff)
             s) < (eval (ENum (0)) s))%Z)),35%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_diff)
             s) >= (eval (ENum (0)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,38%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_best)
             s) < (eval (ENum (0)) s))%Z)),42%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDfind_snap_best)
             s) >= (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,45%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,46%positive)::
             (45%positive,ANone,50%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDfind_snap_best
             (Some (EVar IDfind_snap_diff))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDfind_snap_i
             (Some (EAdd (EVar IDfind_snap_i) (ENum (1))))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDfind_snap_z (Some (EAdd (ENum (1))
             (EVar IDfind_snap_z)))),55%positive)::
             (55%positive,AWeaken,7%positive)::nil
|}.

Definition find_snap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_z) <= 0)%Z
    | 4%positive => (1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 6%positive => (-1 * (s IDfind_snap_i) <= 0 /\ 1 * (s IDfind_snap_i) <= 0 /\ 1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_z) <= 0)%Z
    | 7%positive => (-1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 8%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 9%positive => (-1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 10%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_best) <= 0)%Z
    | 11%positive => (-1 * (s IDfind_snap_best) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 12%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_best) <= 0)%Z
    | 13%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ 1 * (s IDfind_snap_best) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDfind_snap_best) + 1 <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 15%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ 1 * (s IDfind_snap_best) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 17%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 18%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 19%positive => (-1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 20%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 21%positive => (-1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 22%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 23%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ 1 * (s IDfind_snap_adj_dv) <= 0 /\ -1 * (s IDfind_snap_adj_dv) <= 0)%Z
    | 24%positive => (-1 * (s IDfind_snap_adj_dv) <= 0 /\ 1 * (s IDfind_snap_adj_dv) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 25%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 26%positive => (-1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 27%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i)+ 1 * (s IDfind_snap_psst_dref_off0) <= 0)%Z
    | 28%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 30%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 32%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_diff) <= 0)%Z
    | 33%positive => (-1 * (s IDfind_snap_diff) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 34%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_diff) <= 0)%Z
    | 35%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ 1 * (s IDfind_snap_diff) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDfind_snap_diff) + 1 <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 37%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ 1 * (s IDfind_snap_diff) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 39%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_best) <= 0)%Z
    | 40%positive => (-1 * (s IDfind_snap_best) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 41%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_best) <= 0)%Z
    | 42%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ 1 * (s IDfind_snap_best) + 1 <= 0)%Z
    | 43%positive => (1 * (s IDfind_snap_best) + 1 <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 44%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ 1 * (s IDfind_snap_best) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 46%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 48%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 50%positive => (-1 * (s IDfind_snap_i) <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) + 1 <= 0 /\ -1 * (s IDfind_snap_z) <= 0 /\ -1 * (s IDfind_snap_i) <= 0)%Z
    | 52%positive => (-1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_i) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDfind_snap_i) + 1 <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) <= 0)%Z
    | 54%positive => (-1 * (s IDfind_snap_z) <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_i) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDfind_snap_i) + 1 <= 0 /\ 1 * (s IDfind_snap_i)+ -1 * (s IDfind_snap_psst_dref_off0) <= 0 /\ -1 * (s IDfind_snap_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition find_snap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDfind_snap_psst_dref_off0)))%Q
    | 2%positive => ((s IDfind_snap_z) + max0((s IDfind_snap_psst_dref_off0)))%Q
    | 3%positive => ((s IDfind_snap_z) + max0((s IDfind_snap_psst_dref_off0)))%Q
    | 4%positive => ((s IDfind_snap_z) + max0((s IDfind_snap_psst_dref_off0)))%Q
    | 5%positive => ((s IDfind_snap_z)
                     + max0(-(s IDfind_snap_i)
                            + (s IDfind_snap_psst_dref_off0)))%Q
    | 6%positive => ((s IDfind_snap_z)
                     + max0(-(s IDfind_snap_i)
                            + (s IDfind_snap_psst_dref_off0)))%Q
    | 7%positive => ((s IDfind_snap_z)
                     + max0(-(s IDfind_snap_i)
                            + (s IDfind_snap_psst_dref_off0)))%Q
    | 8%positive => ((s IDfind_snap_z)
                     + max0(-(s IDfind_snap_i)
                            + (s IDfind_snap_psst_dref_off0)))%Q
    | 9%positive => ((s IDfind_snap_z)
                     + max0(-(s IDfind_snap_i)
                            + (s IDfind_snap_psst_dref_off0)))%Q
    | 10%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 11%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 12%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 13%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 14%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 15%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 16%positive => ((s IDfind_snap_z))%Q
    | 17%positive => ((s IDfind_snap_z))%Q
    | 18%positive => ((s IDfind_snap_z))%Q
    | 19%positive => ((s IDfind_snap_z))%Q
    | 20%positive => ((s IDfind_snap_z))%Q
    | 21%positive => ((s IDfind_snap_z))%Q
    | 22%positive => ((s IDfind_snap_z))%Q
    | 23%positive => ((s IDfind_snap_z))%Q
    | 24%positive => ((s IDfind_snap_z))%Q
    | 25%positive => ((s IDfind_snap_z))%Q
    | 26%positive => ((s IDfind_snap_z))%Q
    | 27%positive => ((s IDfind_snap_z))%Q
    | 28%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 29%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 30%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 31%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 32%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 33%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 34%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 35%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 36%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 37%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 38%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 39%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 40%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 41%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 42%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 43%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 44%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 45%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 46%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 47%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 48%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 49%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 50%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 51%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-1 - (s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 52%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 53%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 54%positive => ((1 # 1) + (s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | 55%positive => ((s IDfind_snap_z)
                      + max0(-(s IDfind_snap_i)
                             + (s IDfind_snap_psst_dref_off0)))%Q
    | _ => (0 # 1)%Q
  end.

Definition find_snap_hints (p : node) (s : state) := 
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
    | 12%positive => [(*0 1*) F_max0_monotonic (F_check_ge (-(s IDfind_snap_i)
                                                            + (s IDfind_snap_psst_dref_off0)) (-1
                                                                    - (s IDfind_snap_i)
                                                                    + (s IDfind_snap_psst_dref_off0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDfind_snap_i)
                                                                 + (s IDfind_snap_psst_dref_off0))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDfind_snap_i)
                                                             + (s IDfind_snap_psst_dref_off0)) (-1
                                                                    - (s IDfind_snap_i)
                                                                    + (s IDfind_snap_psst_dref_off0)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDfind_snap_i)
                                            + (s IDfind_snap_psst_dref_off0))]
    | 16%positive => []
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
    | 28%positive => [(*0 1*) F_max0_pre_decrement (-(s IDfind_snap_i)
                                                    + (s IDfind_snap_psst_dref_off0)) (1)]
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
    | _ => []
  end.


Theorem find_snap_ai_correct:
  forall s p' s', steps (g_start find_snap) s (g_edges find_snap) p' s' -> find_snap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem find_snap_pot_correct:
  forall s p' s',
    steps (g_start find_snap) s (g_edges find_snap) p' s' ->
    (find_snap_pot (g_start find_snap) s >= find_snap_pot p' s')%Q.
Proof.
  check_lp find_snap_ai_correct find_snap_hints.
Qed.

