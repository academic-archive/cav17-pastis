Require Import pasta.Pasta.

Notation ID_TIFFFindFieldInfo_z := 1%positive.
Notation ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0 := 2%positive.
Notation ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off8 := 3%positive.
Notation ID_TIFFFindFieldInfo__tmp := 4%positive.
Notation ID_TIFFFindFieldInfo__tmp1 := 5%positive.
Notation ID_TIFFFindFieldInfo_i := 6%positive.
Notation ID_TIFFFindFieldInfo_n := 7%positive.
Notation ID_TIFFFindFieldInfo_tif_dref_off848 := 8%positive.
Notation ID_TIFFFindFieldInfo_dt := 9%positive.
Notation ID_TIFFFindFieldInfo_tag := 10%positive.
Notation ID_TIFFFindFieldInfo_tif := 11%positive.
Definition _TIFFFindFieldInfo : graph := {|
  g_start := 1%positive;
  g_end := 41%positive;
  g_edges := (1%positive,(AAssign ID_TIFFFindFieldInfo_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign ID_TIFFFindFieldInfo__tmp
             (Some (EVar ID_TIFFFindFieldInfo_tag))),3%positive)::
             (3%positive,(AAssign ID_TIFFFindFieldInfo__tmp1
             (Some (EVar ID_TIFFFindFieldInfo_dt))),4%positive)::
             (4%positive,AWeaken,5%positive)::(5%positive,ANone,6%positive)::
             (5%positive,ANone,14%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)
             s) = (eval (EVar ID_TIFFFindFieldInfo__tmp) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)
             s) <> (eval (EVar ID_TIFFFindFieldInfo__tmp) s))%Z)),8%positive)::
             (8%positive,AWeaken,14%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__tmp1) s) =
             (eval (ENum (0)) s))%Z)),38%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__tmp1) s) =
             (eval (EVar ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off8)
             s))%Z)),37%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__tmp1) s) <>
             (eval (EVar ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off8)
             s))%Z)),13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign ID_TIFFFindFieldInfo_i (Some (ENum (0)))),
             15%positive)::
             (15%positive,(AAssign ID_TIFFFindFieldInfo_n
             (Some (EVar ID_TIFFFindFieldInfo_tif_dref_off848))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo_i) s) <
             (eval (EVar ID_TIFFFindFieldInfo_n) s))%Z)),22%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo_i) s) >=
             (eval (EVar ID_TIFFFindFieldInfo_n) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,41%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (23%positive,ANone,28%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__tmp1) s) =
             (eval (ENum (0)) s))%Z)),34%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar ID_TIFFFindFieldInfo__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,35%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign ID_TIFFFindFieldInfo_i
             (Some (EAdd (EVar ID_TIFFFindFieldInfo_i) (ENum (1))))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign ID_TIFFFindFieldInfo_z
             (Some (EAdd (ENum (1)) (EVar ID_TIFFFindFieldInfo_z)))),
             33%positive)::(33%positive,AWeaken,18%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,41%positive)::
             (37%positive,AWeaken,39%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::nil
|}.

Definition _TIFFFindFieldInfo_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 3%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 4%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 5%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 6%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 7%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 8%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 9%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0)%Z
    | 10%positive => (-1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 11%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0)%Z
    | 12%positive => (-1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 13%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0)%Z
    | 14%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 15%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0)%Z
    | 16%positive => (-1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 17%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0)%Z
    | 18%positive => (-1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 19%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i)+ 1 * (s ID_TIFFFindFieldInfo_n) <= 0)%Z
    | 20%positive => (-1 * (s ID_TIFFFindFieldInfo_i)+ 1 * (s ID_TIFFFindFieldInfo_n) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 21%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i)+ 1 * (s ID_TIFFFindFieldInfo_n) <= 0)%Z
    | 22%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0)%Z
    | 23%positive => (1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 24%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0)%Z
    | 25%positive => (1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 26%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0)%Z
    | 27%positive => (1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 28%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0)%Z
    | 29%positive => (1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 30%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s ID_TIFFFindFieldInfo_i) + 1 <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 32%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s ID_TIFFFindFieldInfo_i) + 1 <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) + 1 <= 0)%Z
    | 34%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__tmp1) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__tmp1) <= 0)%Z
    | 35%positive => (1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 36%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_i) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_i)+ -1 * (s ID_TIFFFindFieldInfo_n) + 1 <= 0)%Z
    | 37%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off8)+ 1 * (s ID_TIFFFindFieldInfo__tmp1) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off8)+ -1 * (s ID_TIFFFindFieldInfo__tmp1) <= 0)%Z
    | 38%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__tmp1) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__tmp1) <= 0)%Z
    | 39%positive => (-1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | 40%positive => (1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo_z) <= 0 /\ 1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ -1 * (s ID_TIFFFindFieldInfo__tmp) <= 0 /\ -1 * (s ID_TIFFFindFieldInfo__TIFFFindFieldInfo.last_dref_off0)+ 1 * (s ID_TIFFFindFieldInfo__tmp) <= 0)%Z
    | 41%positive => (-1 * (s ID_TIFFFindFieldInfo_z) <= 0)%Z
    | _ => False
  end.

Definition _TIFFFindFieldInfo_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 2%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 3%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 4%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 5%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 6%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 7%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 8%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 9%positive => ((s ID_TIFFFindFieldInfo_z)
                     + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 10%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 11%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 12%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 13%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 14%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 15%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 16%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 17%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 18%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 19%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 20%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 21%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 22%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 23%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 24%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 25%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 26%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 27%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 28%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 29%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 30%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 31%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 32%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 33%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0(-(s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 34%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 35%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 36%positive => ((1 # 1) + (s ID_TIFFFindFieldInfo_z)
                      + max0(-1 - (s ID_TIFFFindFieldInfo_i)
                             + (s ID_TIFFFindFieldInfo_n)))%Q
    | 37%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 38%positive => ((s ID_TIFFFindFieldInfo_z)
                      + max0((s ID_TIFFFindFieldInfo_tif_dref_off848)))%Q
    | 39%positive => ((s ID_TIFFFindFieldInfo_z))%Q
    | 40%positive => ((s ID_TIFFFindFieldInfo_z))%Q
    | 41%positive => ((s ID_TIFFFindFieldInfo_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition _TIFFFindFieldInfo_hints (p : node) (s : state) := 
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
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s ID_TIFFFindFieldInfo_i)
                                                             + (s ID_TIFFFindFieldInfo_n)) (-1
                                                                    - (s ID_TIFFFindFieldInfo_i)
                                                                    + (s ID_TIFFFindFieldInfo_n)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s ID_TIFFFindFieldInfo_i)
                                            + (s ID_TIFFFindFieldInfo_n))]
    | 22%positive => [(*-1 0*) F_max0_pre_decrement (-(s ID_TIFFFindFieldInfo_i)
                                                     + (s ID_TIFFFindFieldInfo_n)) (1)]
    | 23%positive => []
    | 24%positive => []
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
    | 36%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s ID_TIFFFindFieldInfo_i)
                                            + (s ID_TIFFFindFieldInfo_n))]
    | 37%positive => [(*0 1*) F_max0_ge_0 ((s ID_TIFFFindFieldInfo_tif_dref_off848))]
    | 38%positive => [(*0 1*) F_max0_ge_0 ((s ID_TIFFFindFieldInfo_tif_dref_off848))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | _ => []
  end.


Theorem _TIFFFindFieldInfo_ai_correct:
  forall s p' s', steps (g_start _TIFFFindFieldInfo) s (g_edges _TIFFFindFieldInfo) p' s' -> _TIFFFindFieldInfo_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem _TIFFFindFieldInfo_pot_correct:
  forall s p' s',
    steps (g_start _TIFFFindFieldInfo) s (g_edges _TIFFFindFieldInfo) p' s' ->
    (_TIFFFindFieldInfo_pot (g_start _TIFFFindFieldInfo) s >= _TIFFFindFieldInfo_pot p' s')%Q.
Proof.
  check_lp _TIFFFindFieldInfo_ai_correct _TIFFFindFieldInfo_hints.
Qed.

