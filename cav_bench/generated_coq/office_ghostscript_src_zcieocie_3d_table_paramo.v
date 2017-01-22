Require Import pasta.Pasta.

Notation IDcie_3d_table_param_z := 1%positive.
Notation IDcie_3d_table_param__tmp := 2%positive.
Notation IDcie_3d_table_param__tmp1 := 3%positive.
Notation IDcie_3d_table_param__tmp2 := 4%positive.
Notation IDcie_3d_table_param_i := 5%positive.
Notation IDcie_3d_table_param_count := 6%positive.
Notation IDcie_3d_table_param_nbytes := 7%positive.
Notation IDcie_3d_table_param_ptable := 8%positive.
Notation IDcie_3d_table_param_strings := 9%positive.
Definition cie_3d_table_param : graph := {|
  g_start := 1%positive;
  g_end := 51%positive;
  g_edges := (1%positive,(AAssign IDcie_3d_table_param_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcie_3d_table_param_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDcie_3d_table_param__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDcie_3d_table_param__tmp
             (Some (EVar IDcie_3d_table_param_count))),6%positive)::
             (6%positive,(AAssign IDcie_3d_table_param__tmp2
             (Some (EVar IDcie_3d_table_param_nbytes))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,17%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,15%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDcie_3d_table_param__tmp1 None),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,51%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,51%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,48%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcie_3d_table_param_i (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcie_3d_table_param_i) s) <
             (eval (EVar IDcie_3d_table_param__tmp) s))%Z)),27%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcie_3d_table_param_i) s) >=
             (eval (EVar IDcie_3d_table_param__tmp) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDcie_3d_table_param__tmp1
             (Some (ENum (0)))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,51%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,37%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,35%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDcie_3d_table_param__tmp1 None),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,51%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,51%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,45%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDcie_3d_table_param_i
             (Some (EAdd (EVar IDcie_3d_table_param_i) (ENum (1))))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDcie_3d_table_param_z
             (Some (EAdd (ENum (1)) (EVar IDcie_3d_table_param_z)))),
             44%positive)::(44%positive,AWeaken,22%positive)::
             (45%positive,(AAssign IDcie_3d_table_param__tmp1
             (Some (ENum (-15)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,51%positive)::
             (48%positive,(AAssign IDcie_3d_table_param__tmp1
             (Some (ENum (-15)))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,51%positive)::nil
|}.

Definition cie_3d_table_param_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 4%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_3d_table_param__tmp) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 8%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 17%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 18%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 20%positive => (1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 21%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 22%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 23%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp)+ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 24%positive => (1 * (s IDcie_3d_table_param__tmp)+ -1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 25%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp)+ -1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp1) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp1) <= 0)%Z
    | 26%positive => (-1 * (s IDcie_3d_table_param__tmp1) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp1) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp)+ -1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 27%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 29%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 31%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 33%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 35%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 37%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 39%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 41%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | 43%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp1) + 15 <= 0 /\ -1 * (s IDcie_3d_table_param__tmp1) + -15 <= 0)%Z
    | 47%positive => (-1 * (s IDcie_3d_table_param__tmp1) + -15 <= 0 /\ 1 * (s IDcie_3d_table_param__tmp1) + 15 <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param__tmp)+ 1 * (s IDcie_3d_table_param_i) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 49%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param__tmp1) + 15 <= 0 /\ -1 * (s IDcie_3d_table_param__tmp1) + -15 <= 0)%Z
    | 50%positive => (-1 * (s IDcie_3d_table_param__tmp1) + -15 <= 0 /\ 1 * (s IDcie_3d_table_param__tmp1) + 15 <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0 /\ 1 * (s IDcie_3d_table_param_z) <= 0 /\ -1 * (s IDcie_3d_table_param_i) <= 0)%Z
    | 51%positive => (-1 * (s IDcie_3d_table_param_i) <= 0 /\ -1 * (s IDcie_3d_table_param_z) <= 0)%Z
    | _ => False
  end.

Definition cie_3d_table_param_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcie_3d_table_param_count)))%Q
    | 2%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param_count)))%Q
    | 3%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param_count)))%Q
    | 4%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param_count)))%Q
    | 5%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param_count)))%Q
    | 6%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param__tmp)))%Q
    | 7%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param__tmp)))%Q
    | 8%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param__tmp)))%Q
    | 9%positive => ((s IDcie_3d_table_param_z)
                     + max0((s IDcie_3d_table_param__tmp)))%Q
    | 10%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 11%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 12%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 13%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 14%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 15%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 16%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 17%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 18%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 19%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 20%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 21%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 22%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 23%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 24%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 25%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 26%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 27%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 28%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 29%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 30%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 31%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 32%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 33%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 34%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 35%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 36%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 37%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 38%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0(-1 + (s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 39%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0(-1 + (s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 40%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0(-1 + (s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 41%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 42%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 43%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 44%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 45%positive => ((1 # 1) + (s IDcie_3d_table_param_z)
                      + max0(-1 + (s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i)))%Q
    | 46%positive => ((s IDcie_3d_table_param_z)
                      + max0(-1 + (s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i))
                      + (1 # 15) * max0(-(s IDcie_3d_table_param__tmp1)))%Q
    | 47%positive => ((s IDcie_3d_table_param_z)
                      + max0(-1 + (s IDcie_3d_table_param__tmp)
                             - (s IDcie_3d_table_param_i))
                      + (1 # 15) * max0(-(s IDcie_3d_table_param__tmp1)))%Q
    | 48%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 49%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 50%positive => ((s IDcie_3d_table_param_z)
                      + max0((s IDcie_3d_table_param__tmp)))%Q
    | 51%positive => ((s IDcie_3d_table_param_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_3d_table_param_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-1 0*) F_max0_ge_0 ((s IDcie_3d_table_param__tmp))]
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_ge_0 ((s IDcie_3d_table_param__tmp))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcie_3d_table_param__tmp)
                                                             - (s IDcie_3d_table_param_i)) (-1
                                                                    + (s IDcie_3d_table_param__tmp)
                                                                    - (s IDcie_3d_table_param_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcie_3d_table_param__tmp)
                                            - (s IDcie_3d_table_param_i))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDcie_3d_table_param__tmp)
                                                     - (s IDcie_3d_table_param_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcie_3d_table_param__tmp)
                                            - (s IDcie_3d_table_param_i))]
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDcie_3d_table_param__tmp)
                                                     - (s IDcie_3d_table_param_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcie_3d_table_param__tmp)
                                            - (s IDcie_3d_table_param_i))]
    | 37%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcie_3d_table_param__tmp)
                                                     - (s IDcie_3d_table_param_i)) (1)]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcie_3d_table_param__tmp)
                                            - (s IDcie_3d_table_param_i));
                      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDcie_3d_table_param__tmp1))) (F_check_ge (0) (0))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_max0_ge_0 ((s IDcie_3d_table_param__tmp))]
    | 51%positive => []
    | _ => []
  end.


Theorem cie_3d_table_param_ai_correct:
  forall s p' s', steps (g_start cie_3d_table_param) s (g_edges cie_3d_table_param) p' s' -> cie_3d_table_param_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_3d_table_param_pot_correct:
  forall s p' s',
    steps (g_start cie_3d_table_param) s (g_edges cie_3d_table_param) p' s' ->
    (cie_3d_table_param_pot (g_start cie_3d_table_param) s >= cie_3d_table_param_pot p' s')%Q.
Proof.
  check_lp cie_3d_table_param_ai_correct cie_3d_table_param_hints.
Qed.

