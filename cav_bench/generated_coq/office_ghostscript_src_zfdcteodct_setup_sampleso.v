Require Import pasta.Pasta.

Notation IDdct_setup_samples_z := 1%positive.
Notation IDdct_setup_samples__tmp := 2%positive.
Notation IDdct_setup_samples__tmp1 := 3%positive.
Notation IDdct_setup_samples__tmp2 := 4%positive.
Notation IDdct_setup_samples_code := 5%positive.
Notation IDdct_setup_samples_i := 6%positive.
Notation IDdct_setup_samples_is_vert := 7%positive.
Notation IDdct_setup_samples_jcdp := 8%positive.
Notation IDdct_setup_samples_kstr := 9%positive.
Notation IDdct_setup_samples_num_colors := 10%positive.
Notation IDdct_setup_samples_op := 11%positive.
Definition dct_setup_samples : graph := {|
  g_start := 1%positive;
  g_end := 48%positive;
  g_edges := (1%positive,(AAssign IDdct_setup_samples_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDdct_setup_samples__tmp
             (Some (EVar IDdct_setup_samples_num_colors))),3%positive)::
             (3%positive,(AAssign IDdct_setup_samples__tmp2
             (Some (EVar IDdct_setup_samples_is_vert))),4%positive)::
             (4%positive,AWeaken,5%positive)::(5%positive,ANone,6%positive)::
             (5%positive,ANone,8%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,ANone,9%positive)::(7%positive,ANone,8%positive)::
             (8%positive,ANone,15%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,45%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDdct_setup_samples_code None),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,ANone,42%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDdct_setup_samples_i (Some (ENum (0)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDdct_setup_samples_i) s) <
             (eval (EVar IDdct_setup_samples__tmp) s))%Z)),23%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDdct_setup_samples_i) s) >=
             (eval (EVar IDdct_setup_samples__tmp) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign IDdct_setup_samples__tmp1
             (Some (ENum (0)))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,48%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,39%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,39%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDdct_setup_samples__tmp2) s) <>
             (eval (ENum (0)) s))%Z)),31%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDdct_setup_samples__tmp2) s) =
             (eval (ENum (0)) s))%Z)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,33%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDdct_setup_samples_i
             (Some (EAdd (EVar IDdct_setup_samples_i) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDdct_setup_samples_z
             (Some (EAdd (ENum (1)) (EVar IDdct_setup_samples_z)))),
             38%positive)::(38%positive,AWeaken,18%positive)::
             (39%positive,(AAssign IDdct_setup_samples__tmp1
             (Some (ENum (-15)))),40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,48%positive)::
             (42%positive,(AAssign IDdct_setup_samples__tmp1
             (Some (EVar IDdct_setup_samples_code))),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,48%positive)::
             (45%positive,(AAssign IDdct_setup_samples__tmp1
             (Some (ENum (-15)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::nil
|}.

Definition dct_setup_samples_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 4%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 5%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 6%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 7%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 8%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 9%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 10%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 11%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 12%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 13%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 14%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 15%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 16%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 17%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ 1 * (s IDdct_setup_samples_i) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 18%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 19%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples__tmp)+ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 20%positive => (1 * (s IDdct_setup_samples__tmp)+ -1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 21%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples__tmp)+ -1 * (s IDdct_setup_samples_i) <= 0 /\ 1 * (s IDdct_setup_samples__tmp1) <= 0 /\ -1 * (s IDdct_setup_samples__tmp1) <= 0)%Z
    | 22%positive => (-1 * (s IDdct_setup_samples__tmp1) <= 0 /\ 1 * (s IDdct_setup_samples__tmp1) <= 0 /\ 1 * (s IDdct_setup_samples__tmp)+ -1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 23%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 25%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 27%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 29%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ 1 * (s IDdct_setup_samples__tmp2) <= 0 /\ -1 * (s IDdct_setup_samples__tmp2) <= 0)%Z
    | 30%positive => (-1 * (s IDdct_setup_samples__tmp2) <= 0 /\ 1 * (s IDdct_setup_samples__tmp2) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 31%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 33%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0)%Z
    | 35%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 37%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0 /\ 1 * (s IDdct_setup_samples__tmp1) + 15 <= 0 /\ -1 * (s IDdct_setup_samples__tmp1) + -15 <= 0)%Z
    | 41%positive => (-1 * (s IDdct_setup_samples__tmp1) + -15 <= 0 /\ 1 * (s IDdct_setup_samples__tmp1) + 15 <= 0 /\ -1 * (s IDdct_setup_samples_i) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples__tmp)+ 1 * (s IDdct_setup_samples_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 43%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 44%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 45%positive => (1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 46%positive => (-1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0 /\ 1 * (s IDdct_setup_samples__tmp1) + 15 <= 0 /\ -1 * (s IDdct_setup_samples__tmp1) + -15 <= 0)%Z
    | 47%positive => (-1 * (s IDdct_setup_samples__tmp1) + -15 <= 0 /\ 1 * (s IDdct_setup_samples__tmp1) + 15 <= 0 /\ 1 * (s IDdct_setup_samples_z) <= 0 /\ -1 * (s IDdct_setup_samples_z) <= 0)%Z
    | 48%positive => (-1 * (s IDdct_setup_samples_z) <= 0)%Z
    | _ => False
  end.

Definition dct_setup_samples_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDdct_setup_samples_num_colors)))%Q
    | 2%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples_num_colors)))%Q
    | 3%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 4%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 5%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 6%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 7%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 8%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 9%positive => ((s IDdct_setup_samples_z)
                     + max0((s IDdct_setup_samples__tmp)))%Q
    | 10%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 11%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 12%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 13%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 14%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 15%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 16%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 17%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 18%positive => (max0((s IDdct_setup_samples__tmp)
                           - (s IDdct_setup_samples_i))
                      + max0((s IDdct_setup_samples_z)))%Q
    | 19%positive => (max0((s IDdct_setup_samples__tmp)
                           - (s IDdct_setup_samples_i))
                      + max0((s IDdct_setup_samples_z)))%Q
    | 20%positive => (max0((s IDdct_setup_samples__tmp)
                           - (s IDdct_setup_samples_i))
                      + max0((s IDdct_setup_samples_z)))%Q
    | 21%positive => (max0((s IDdct_setup_samples__tmp)
                           - (s IDdct_setup_samples_i))
                      + max0((s IDdct_setup_samples_z)))%Q
    | 22%positive => (max0((s IDdct_setup_samples__tmp)
                           - (s IDdct_setup_samples_i))
                      + max0((s IDdct_setup_samples_z)))%Q
    | 23%positive => (max0((s IDdct_setup_samples__tmp)
                           - (s IDdct_setup_samples_i))
                      + max0((s IDdct_setup_samples_z)))%Q
    | 24%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 25%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 26%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 27%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 28%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 29%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 30%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 31%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 32%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 33%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 34%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 35%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 36%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 37%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 38%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 39%positive => ((1 # 1) + (s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i)))%Q
    | 40%positive => ((s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i))
                      + (1 # 15) * max0(-(s IDdct_setup_samples__tmp1)))%Q
    | 41%positive => ((s IDdct_setup_samples_z)
                      + max0(-1 + (s IDdct_setup_samples__tmp)
                             - (s IDdct_setup_samples_i))
                      + (1 # 15) * max0(-(s IDdct_setup_samples__tmp1)))%Q
    | 42%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 43%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 44%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 45%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 46%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 47%positive => ((s IDdct_setup_samples_z)
                      + max0((s IDdct_setup_samples__tmp)))%Q
    | 48%positive => ((s IDdct_setup_samples_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition dct_setup_samples_hints (p : node) (s : state) := 
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
    | 17%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdct_setup_samples_z)) (0))) (F_max0_ge_0 ((s IDdct_setup_samples_z)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDdct_setup_samples__tmp)
                                                             - (s IDdct_setup_samples_i)) (-1
                                                                    + (s IDdct_setup_samples__tmp)
                                                                    - (s IDdct_setup_samples_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDdct_setup_samples__tmp)
                                            - (s IDdct_setup_samples_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDdct_setup_samples_z))) (F_check_ge ((s IDdct_setup_samples_z)) (0))]
    | 23%positive => [(*-1 0*) F_max0_pre_decrement ((s IDdct_setup_samples__tmp)
                                                     - (s IDdct_setup_samples_i)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDdct_setup_samples_z))) (F_check_ge ((s IDdct_setup_samples_z)) (0))]
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
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdct_setup_samples_z)) (0))) (F_max0_ge_0 ((s IDdct_setup_samples_z)))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDdct_setup_samples__tmp)
                                            - (s IDdct_setup_samples_i));
                      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDdct_setup_samples__tmp1))) (F_check_ge (0) (0))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_max0_ge_0 ((s IDdct_setup_samples__tmp))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_max0_ge_0 ((s IDdct_setup_samples__tmp))]
    | 48%positive => []
    | _ => []
  end.


Theorem dct_setup_samples_ai_correct:
  forall s p' s', steps (g_start dct_setup_samples) s (g_edges dct_setup_samples) p' s' -> dct_setup_samples_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem dct_setup_samples_pot_correct:
  forall s p' s',
    steps (g_start dct_setup_samples) s (g_edges dct_setup_samples) p' s' ->
    (dct_setup_samples_pot (g_start dct_setup_samples) s >= dct_setup_samples_pot p' s')%Q.
Proof.
  check_lp dct_setup_samples_ai_correct dct_setup_samples_hints.
Qed.

