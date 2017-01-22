Require Import pasta.Pasta.

Notation IDarray_get_z := 1%positive.
Notation IDarray_get__tmp := 2%positive.
Notation IDarray_get__tmp1 := 3%positive.
Notation IDarray_get_aref_dref_off0_off2 := 4%positive.
Notation IDarray_get_index := 5%positive.
Notation IDarray_get_aref := 6%positive.
Notation IDarray_get_index_long := 7%positive.
Notation IDarray_get_pref := 8%positive.
Definition array_get : graph := {|
  g_start := 1%positive;
  g_end := 39%positive;
  g_edges := (1%positive,(AAssign IDarray_get_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDarray_get_aref_dref_off0_off2) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDarray_get__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDarray_get__tmp
             (Some (EVar IDarray_get_index_long))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDarray_get__tmp)
             s) >= (eval (EVar IDarray_get_aref_dref_off0_off2) s))%Z)),
             35%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDarray_get__tmp)
             s) < (eval (EVar IDarray_get_aref_dref_off0_off2) s))%Z)),
             8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,ANone,32%positive)::(9%positive,ANone,29%positive)::
             (9%positive,ANone,13%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDarray_get__tmp1 (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,39%positive)::
             (13%positive,(AAssign IDarray_get_index
             (Some (EVar IDarray_get__tmp))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDarray_get_index
             (Some (EAdd (EVar IDarray_get_index) (ENum (-1))))),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDarray_get_index)
             s) <> (eval (ENum (0)) s))%Z)),22%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDarray_get_index)
             s) = (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDarray_get__tmp1 (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,39%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,25%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,26%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDarray_get_z (Some (EAdd (ENum (1))
             (EVar IDarray_get_z)))),15%positive)::
             (29%positive,(AAssign IDarray_get__tmp1 (Some (ENum (0)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,39%positive)::
             (32%positive,(AAssign IDarray_get__tmp1 (Some (ENum (-20)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,39%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDarray_get__tmp1 (Some (ENum (-15)))),
             37%positive)::(37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::nil
|}.

Definition array_get_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0)%Z
    | 3%positive => (-1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 4%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDarray_get__tmp) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 6%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0)%Z
    | 7%positive => (-1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 8%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 9%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 10%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get__tmp1) <= 0)%Z
    | 12%positive => (-1 * (s IDarray_get__tmp1) <= 0 /\ 1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 16%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0)%Z
    | 17%positive => (-1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 18%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ 1 * (s IDarray_get_index) <= 0 /\ -1 * (s IDarray_get_index) <= 0)%Z
    | 19%positive => (-1 * (s IDarray_get_index) <= 0 /\ 1 * (s IDarray_get_index) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ 1 * (s IDarray_get_index) <= 0 /\ -1 * (s IDarray_get_index) <= 0 /\ 1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get__tmp1) <= 0)%Z
    | 21%positive => (-1 * (s IDarray_get__tmp1) <= 0 /\ 1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get_index) <= 0 /\ 1 * (s IDarray_get_index) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0)%Z
    | 23%positive => (-1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 24%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0)%Z
    | 25%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0)%Z
    | 26%positive => (-1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 27%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0)%Z
    | 28%positive => (-1 * (s IDarray_get_aref_dref_off0_off2)+ 1 * (s IDarray_get_index) + 2 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 29%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get__tmp1) <= 0)%Z
    | 31%positive => (-1 * (s IDarray_get__tmp1) <= 0 /\ 1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp1) + 20 <= 0 /\ -1 * (s IDarray_get__tmp1) + -20 <= 0)%Z
    | 34%positive => (-1 * (s IDarray_get__tmp1) + -20 <= 0 /\ 1 * (s IDarray_get__tmp1) + 20 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get__tmp)+ -1 * (s IDarray_get_aref_dref_off0_off2) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get__tmp)+ 1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 36%positive => (-1 * (s IDarray_get__tmp)+ 1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 37%positive => (-1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get__tmp)+ 1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ 1 * (s IDarray_get__tmp1) + 15 <= 0 /\ -1 * (s IDarray_get__tmp1) + -15 <= 0)%Z
    | 38%positive => (-1 * (s IDarray_get__tmp1) + -15 <= 0 /\ 1 * (s IDarray_get__tmp1) + 15 <= 0 /\ -1 * (s IDarray_get__tmp)+ 1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ -1 * (s IDarray_get_z) <= 0 /\ 1 * (s IDarray_get_z) <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0)%Z
    | 39%positive => (1 * (s IDarray_get__tmp1) <= 0 /\ -1 * (s IDarray_get__tmp1) + -20 <= 0 /\ -1 * (s IDarray_get_aref_dref_off0_off2) <= 0 /\ -1 * (s IDarray_get_z) <= 0)%Z
    | _ => False
  end.

Definition array_get_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 2%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 3%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 4%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 5%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 6%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 7%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 8%positive => ((s IDarray_get_z)
                     + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 9%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                     + max0(-1 - (s IDarray_get__tmp)
                            + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 10%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 11%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 12%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 13%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 14%positive => ((1 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 15%positive => ((1 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 16%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 17%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 18%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 19%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 20%positive => ((s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2))
                      + (2 # 15) * max0(15 + (s IDarray_get__tmp1)))%Q
    | 21%positive => ((s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2))
                      + (2 # 15) * max0(15 + (s IDarray_get__tmp1)))%Q
    | 22%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 23%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 24%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 25%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 26%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 27%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 28%positive => ((2 # 1) + (s IDarray_get_index) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 29%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 30%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 31%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 32%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 33%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 34%positive => ((1 # 1) + (s IDarray_get__tmp) + (s IDarray_get_z)
                      + max0(-1 - (s IDarray_get__tmp)
                             + (s IDarray_get_aref_dref_off0_off2)))%Q
    | 35%positive => ((s IDarray_get_z)
                      + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 36%positive => ((s IDarray_get_z)
                      + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 37%positive => ((s IDarray_get_z)
                      + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 38%positive => ((s IDarray_get_z)
                      + max0((s IDarray_get_aref_dref_off0_off2)))%Q
    | 39%positive => ((s IDarray_get_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition array_get_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDarray_get_aref_dref_off0_off2))) (F_check_ge ((s IDarray_get_aref_dref_off0_off2)) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDarray_get__tmp)
                                                                    + (s IDarray_get_aref_dref_off0_off2)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDarray_get__tmp)
                                                                    + (s IDarray_get_aref_dref_off0_off2)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDarray_get_aref_dref_off0_off2)) (0))) (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   - 
                                                                   (s IDarray_get__tmp)
                                                                   + 
                                                                   (s IDarray_get_aref_dref_off0_off2))) (F_check_ge (-1
                                                                    - (s IDarray_get__tmp)
                                                                    + (s IDarray_get_aref_dref_off0_off2)) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDarray_get_index))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDarray_get_index)) (0))) (F_max0_ge_0 ((s IDarray_get_index)));
                      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    + (s IDarray_get__tmp1))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDarray_get__tmp)
                                                                 + (s IDarray_get_aref_dref_off0_off2))) (F_check_ge (0) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDarray_get_aref_dref_off0_off2)) (0))) (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   - 
                                                                   (s IDarray_get__tmp)
                                                                   + 
                                                                   (s IDarray_get_aref_dref_off0_off2))) (F_check_ge (-1
                                                                    - (s IDarray_get__tmp)
                                                                    + (s IDarray_get_aref_dref_off0_off2)) (0))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDarray_get_aref_dref_off0_off2)) (0))) (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  - (s IDarray_get__tmp)
                                                                  + (s IDarray_get_aref_dref_off0_off2))) (F_check_ge (-1
                                                                    - (s IDarray_get__tmp)
                                                                    + (s IDarray_get_aref_dref_off0_off2)) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDarray_get_aref_dref_off0_off2))) (F_check_ge (0) (0))]
    | 39%positive => []
    | _ => []
  end.


Theorem array_get_ai_correct:
  forall s p' s', steps (g_start array_get) s (g_edges array_get) p' s' -> array_get_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem array_get_pot_correct:
  forall s p' s',
    steps (g_start array_get) s (g_edges array_get) p' s' ->
    (array_get_pot (g_start array_get) s >= array_get_pot p' s')%Q.
Proof.
  check_lp array_get_ai_correct array_get_hints.
Qed.

