Require Import pasta.Pasta.

Notation IDcompute_snaps_z := 1%positive.
Notation IDcompute_snaps__tmp := 2%positive.
Notation IDcompute_snaps__tmp1 := 3%positive.
Notation IDcompute_snaps_code := 4%positive.
Notation IDcompute_snaps_i := 5%positive.
Notation IDcompute_snaps_j := 6%positive.
Notation IDcompute_snaps_psst_dref_off0 := 7%positive.
Notation IDcompute_snaps_pst_dref_off0 := 8%positive.
Notation IDcompute_snaps_from_y := 9%positive.
Notation IDcompute_snaps_pmat := 10%positive.
Notation IDcompute_snaps_psst := 11%positive.
Notation IDcompute_snaps_pst := 12%positive.
Notation IDcompute_snaps_tname := 13%positive.
Notation IDcompute_snaps_to_y := 14%positive.
Definition compute_snaps : graph := {|
  g_start := 1%positive;
  g_end := 18%positive;
  g_edges := (1%positive,(AAssign IDcompute_snaps_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcompute_snaps__tmp1
             (Some (EVar IDcompute_snaps_from_y))),3%positive)::
             (3%positive,(AAssign IDcompute_snaps__tmp
             (Some (EVar IDcompute_snaps_to_y))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDcompute_snaps__tmp)
             s) <> (eval (ENum (0)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDcompute_snaps__tmp)
             s) = (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,10%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDcompute_snaps_j
             (Some (EVar IDcompute_snaps_psst_dref_off0))),11%positive)::
             (11%positive,(AAssign IDcompute_snaps_i (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDcompute_snaps_i)
             s) < (eval (EVar IDcompute_snaps_pst_dref_off0) s))%Z)),
             19%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDcompute_snaps_i)
             s) >= (eval (EVar IDcompute_snaps_pst_dref_off0) s))%Z)),
             15%positive)::(15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDcompute_snaps_psst_dref_off0
             (Some (EVar IDcompute_snaps_j))),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_snaps__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),23%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_snaps__tmp1) s) =
             (eval (ENum (0)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,25%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDcompute_snaps_code None),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_snaps_code) s) <
             (eval (ENum (0)) s))%Z)),36%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_snaps_code) s) >=
             (eval (ENum (0)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,31%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,32%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDcompute_snaps_j
             (Some (EAdd (EVar IDcompute_snaps_j) (ENum (1))))),35%positive)::
             (35%positive,ANone,38%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDcompute_snaps_i
             (Some (EAdd (EVar IDcompute_snaps_i) (ENum (1))))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDcompute_snaps_z (Some (EAdd (ENum (1))
             (EVar IDcompute_snaps_z)))),42%positive)::
             (42%positive,AWeaken,14%positive)::nil
|}.

Definition compute_snaps_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_z) <= 0)%Z
    | 4%positive => (1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_z) <= 0)%Z
    | 6%positive => (1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps__tmp) <= 0 /\ -1 * (s IDcompute_snaps__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDcompute_snaps__tmp) <= 0 /\ 1 * (s IDcompute_snaps__tmp) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_z) <= 0)%Z
    | 8%positive => (1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_z) <= 0)%Z
    | 10%positive => (1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_z) <= 0)%Z
    | 12%positive => (1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 13%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ 1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 15%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i)+ 1 * (s IDcompute_snaps_pst_dref_off0) <= 0)%Z
    | 16%positive => (-1 * (s IDcompute_snaps_i)+ 1 * (s IDcompute_snaps_pst_dref_off0) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 17%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i)+ 1 * (s IDcompute_snaps_pst_dref_off0) <= 0)%Z
    | 18%positive => (-1 * (s IDcompute_snaps_i)+ 1 * (s IDcompute_snaps_pst_dref_off0) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 19%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 21%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ 1 * (s IDcompute_snaps__tmp1) <= 0 /\ -1 * (s IDcompute_snaps__tmp1) <= 0)%Z
    | 22%positive => (-1 * (s IDcompute_snaps__tmp1) <= 0 /\ 1 * (s IDcompute_snaps__tmp1) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 23%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 25%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 27%positive => (-1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_code) <= 0)%Z
    | 29%positive => (-1 * (s IDcompute_snaps_code) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_code) <= 0)%Z
    | 31%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_code) <= 0)%Z
    | 32%positive => (-1 * (s IDcompute_snaps_code) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_code) <= 0)%Z
    | 34%positive => (-1 * (s IDcompute_snaps_code) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_code) <= 0)%Z
    | 36%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ 1 * (s IDcompute_snaps_code) + 1 <= 0)%Z
    | 37%positive => (1 * (s IDcompute_snaps_code) + 1 <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) + 1 <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0 /\ -1 * (s IDcompute_snaps_i) <= 0)%Z
    | 39%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) <= 0 /\ -1 * (s IDcompute_snaps_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDcompute_snaps_i) + 1 <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) <= 0 /\ -1 * (s IDcompute_snaps_z) <= 0)%Z
    | 41%positive => (-1 * (s IDcompute_snaps_z) <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) <= 0 /\ -1 * (s IDcompute_snaps_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDcompute_snaps_i) + 1 <= 0 /\ 1 * (s IDcompute_snaps_i)+ -1 * (s IDcompute_snaps_pst_dref_off0) <= 0 /\ -1 * (s IDcompute_snaps_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition compute_snaps_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 2%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 3%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 4%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 5%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 6%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 7%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 8%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 9%positive => ((s IDcompute_snaps_z)
                     + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 10%positive => ((s IDcompute_snaps_z)
                      + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 11%positive => ((s IDcompute_snaps_z)
                      + max0((s IDcompute_snaps_pst_dref_off0)))%Q
    | 12%positive => ((s IDcompute_snaps_z)
                      + max0(-(s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 13%positive => ((s IDcompute_snaps_z)
                      + max0(-(s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 14%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 15%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 16%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 17%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 18%positive => ((s IDcompute_snaps_z))%Q
    | 19%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 20%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 21%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 22%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 23%positive => (max0(-(s IDcompute_snaps_i)
                           + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 24%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 25%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 26%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 27%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 28%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 29%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 30%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 31%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 32%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 33%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 34%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 35%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 36%positive => ((1 # 1)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0))
                      + max0((s IDcompute_snaps_z)))%Q
    | 37%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 38%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-1 - (s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 39%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-(s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 40%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-(s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 41%positive => ((1 # 1) + (s IDcompute_snaps_z)
                      + max0(-(s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | 42%positive => ((s IDcompute_snaps_z)
                      + max0(-(s IDcompute_snaps_i)
                             + (s IDcompute_snaps_pst_dref_off0)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compute_snaps_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompute_snaps_z)) (0))) (F_max0_ge_0 ((s IDcompute_snaps_z)))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDcompute_snaps_i)
                                                             + (s IDcompute_snaps_pst_dref_off0)) (-1
                                                                    - (s IDcompute_snaps_i)
                                                                    + (s IDcompute_snaps_pst_dref_off0)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDcompute_snaps_i)
                                            + (s IDcompute_snaps_pst_dref_off0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcompute_snaps_z))) (F_check_ge ((s IDcompute_snaps_z)) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*0 1*) F_max0_pre_decrement (-(s IDcompute_snaps_i)
                                                    + (s IDcompute_snaps_pst_dref_off0)) (1)]
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDcompute_snaps_i)
                                                     + (s IDcompute_snaps_pst_dref_off0)) (1)]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcompute_snaps_z))) (F_check_ge ((s IDcompute_snaps_z)) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcompute_snaps_z))) (F_check_ge ((s IDcompute_snaps_z)) (0))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompute_snaps_z)) (0))) (F_max0_ge_0 ((s IDcompute_snaps_z)))]
    | _ => []
  end.


Theorem compute_snaps_ai_correct:
  forall s p' s', steps (g_start compute_snaps) s (g_edges compute_snaps) p' s' -> compute_snaps_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compute_snaps_pot_correct:
  forall s p' s',
    steps (g_start compute_snaps) s (g_edges compute_snaps) p' s' ->
    (compute_snaps_pot (g_start compute_snaps) s >= compute_snaps_pot p' s')%Q.
Proof.
  check_lp compute_snaps_ai_correct compute_snaps_hints.
Qed.

