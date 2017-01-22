Require Import pasta.Pasta.

Notation IDzsetcacheparams_z := 1%positive.
Notation IDzsetcacheparams__tmp := 2%positive.
Notation IDzsetcacheparams_code := 3%positive.
Notation IDzsetcacheparams_i := 4%positive.
Notation IDzsetcacheparams_op := 5%positive.
Definition zsetcacheparams : graph := {|
  g_start := 1%positive;
  g_end := 54%positive;
  g_edges := (1%positive,(AAssign IDzsetcacheparams_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDzsetcacheparams_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDzsetcacheparams_i)
             s) < (eval (ENum (3)) s))%Z)),7%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDzsetcacheparams_i)
             s) >= (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,12%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => True)),38%positive)::
             (10%positive,(AGuard (fun s => True)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,35%positive)::
             (12%positive,ANone,13%positive)::
             (12%positive,ANone,17%positive)::
             (12%positive,ANone,21%positive)::
             (12%positive,ANone,25%positive)::
             (13%positive,(AAssign IDzsetcacheparams_code None),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,32%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDzsetcacheparams_code None),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,29%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDzsetcacheparams_code None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,26%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,35%positive)::
             (26%positive,(AAssign IDzsetcacheparams__tmp
             (Some (EVar IDzsetcacheparams_code))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,54%positive)::
             (29%positive,(AAssign IDzsetcacheparams__tmp
             (Some (EVar IDzsetcacheparams_code))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,54%positive)::
             (32%positive,(AAssign IDzsetcacheparams__tmp
             (Some (EVar IDzsetcacheparams_code))),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,54%positive)::
             (35%positive,(AAssign IDzsetcacheparams__tmp None),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,54%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,43%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDzsetcacheparams__tmp None),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,54%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,51%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDzsetcacheparams_i
             (Some (EAdd (EVar IDzsetcacheparams_i) (ENum (1))))),
             47%positive)::(47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDzsetcacheparams_z (Some (EAdd (ENum (1))
             (EVar IDzsetcacheparams_z)))),50%positive)::
             (50%positive,AWeaken,5%positive)::
             (51%positive,(AAssign IDzsetcacheparams__tmp
             (Some (ENum (-15)))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::nil
|}.

Definition zsetcacheparams_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 4%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ 1 * (s IDzsetcacheparams_i) <= 0 /\ 1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 6%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) + 3 <= 0)%Z
    | 7%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 8%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 9%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 10%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 11%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 12%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 13%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 14%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 15%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 16%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 17%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 18%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 19%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 20%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 21%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 22%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 23%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 24%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 25%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 26%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 27%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 29%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 30%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 31%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 32%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 33%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 34%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 35%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 36%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0)%Z
    | 37%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 38%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 39%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 40%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 41%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 42%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 43%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 44%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 45%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 46%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | 47%positive => (-1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_i) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDzsetcacheparams_i) + 1 <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0)%Z
    | 49%positive => (-1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_i) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDzsetcacheparams_i) + 1 <= 0 /\ 1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 52%positive => (1 * (s IDzsetcacheparams_i) + -2 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0 /\ 1 * (s IDzsetcacheparams__tmp) + 15 <= 0 /\ -1 * (s IDzsetcacheparams__tmp) + -15 <= 0)%Z
    | 53%positive => (-1 * (s IDzsetcacheparams__tmp) + -15 <= 0 /\ 1 * (s IDzsetcacheparams__tmp) + 15 <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ 1 * (s IDzsetcacheparams_i) + -2 <= 0)%Z
    | 54%positive => (1 * (s IDzsetcacheparams_i) + -3 <= 0 /\ -1 * (s IDzsetcacheparams_z) <= 0 /\ -1 * (s IDzsetcacheparams_i) <= 0)%Z
    | _ => False
  end.

Definition zsetcacheparams_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((3 # 1))%Q
    | 2%positive => ((3 # 1) + max0((s IDzsetcacheparams_z)))%Q
    | 3%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 4%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 5%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 6%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 7%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 8%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 9%positive => (max0(3 - (s IDzsetcacheparams_i))
                     + max0((s IDzsetcacheparams_z)))%Q
    | 10%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 11%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 12%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 13%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 14%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 15%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 16%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 17%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 18%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 19%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 20%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 21%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 22%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 23%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 24%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 25%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 26%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 27%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 28%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 29%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 30%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 31%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 32%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 33%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 34%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 35%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 36%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 37%positive => (max0(3 - (s IDzsetcacheparams_i))
                      + max0((s IDzsetcacheparams_z)))%Q
    | 38%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 39%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 40%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 41%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 42%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 43%positive => ((s IDzsetcacheparams_z)
                      + max0(3 - (s IDzsetcacheparams_i)))%Q
    | 44%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 45%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 46%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 47%positive => ((4 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 48%positive => ((4 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 49%positive => ((4 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 50%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 51%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 52%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 53%positive => ((3 # 1) - (s IDzsetcacheparams_i)
                      + (s IDzsetcacheparams_z))%Q
    | 54%positive => ((s IDzsetcacheparams_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zsetcacheparams_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzsetcacheparams_z))) (F_check_ge ((s IDzsetcacheparams_z)) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzsetcacheparams_z)) (0))) (F_max0_ge_0 ((s IDzsetcacheparams_z)))]
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
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDzsetcacheparams_i)) (2
                                                                    - (s IDzsetcacheparams_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDzsetcacheparams_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzsetcacheparams_z))) (F_check_ge ((s IDzsetcacheparams_z)) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDzsetcacheparams_i)) (2
                                                                    - (s IDzsetcacheparams_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDzsetcacheparams_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzsetcacheparams_z))) (F_check_ge ((s IDzsetcacheparams_z)) (0))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDzsetcacheparams_i)) (2
                                                                    - (s IDzsetcacheparams_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDzsetcacheparams_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzsetcacheparams_z))) (F_check_ge ((s IDzsetcacheparams_z)) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDzsetcacheparams_i)) (2
                                                                    - (s IDzsetcacheparams_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDzsetcacheparams_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzsetcacheparams_z))) (F_check_ge ((s IDzsetcacheparams_z)) (0))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement (3
                                                     - (s IDzsetcacheparams_i)) (1);
                      (*-1 0*) F_max0_ge_0 (2 - (s IDzsetcacheparams_i))]
    | 43%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                   - 
                                                                   (s IDzsetcacheparams_i))) (F_check_ge (3
                                                                    - (s IDzsetcacheparams_i)) (0))]
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzsetcacheparams_z)) (0))) (F_max0_ge_0 ((s IDzsetcacheparams_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDzsetcacheparams_i)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDzsetcacheparams_i)))]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDzsetcacheparams_i)) (2
                                                                    - (s IDzsetcacheparams_i)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDzsetcacheparams_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDzsetcacheparams_i)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDzsetcacheparams_i)))]
    | 54%positive => []
    | _ => []
  end.


Theorem zsetcacheparams_ai_correct:
  forall s p' s', steps (g_start zsetcacheparams) s (g_edges zsetcacheparams) p' s' -> zsetcacheparams_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zsetcacheparams_pot_correct:
  forall s p' s',
    steps (g_start zsetcacheparams) s (g_edges zsetcacheparams) p' s' ->
    (zsetcacheparams_pot (g_start zsetcacheparams) s >= zsetcacheparams_pot p' s')%Q.
Proof.
  check_lp zsetcacheparams_ai_correct zsetcacheparams_hints.
Qed.

