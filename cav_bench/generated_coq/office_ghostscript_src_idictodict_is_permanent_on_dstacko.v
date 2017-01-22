Require Import pasta.Pasta.

Notation IDdict_is_permanent_on_dstack_z := 1%positive.
Notation IDdict_is_permanent_on_dstack__tmp := 2%positive.
Notation IDdict_is_permanent_on_dstack_count := 3%positive.
Notation IDdict_is_permanent_on_dstack_i := 4%positive.
Notation IDdict_is_permanent_on_dstack_min_dstack_size := 5%positive.
Notation IDdict_is_permanent_on_dstack_pdref := 6%positive.
Definition dict_is_permanent_on_dstack : graph := {|
  g_start := 1%positive;
  g_end := 45%positive;
  g_edges := (1%positive,(AAssign IDdict_is_permanent_on_dstack_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_min_dstack_size)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_count)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,ANone,25%positive)::(6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDdict_is_permanent_on_dstack_count None),
             8%positive)::
             (8%positive,(AAssign IDdict_is_permanent_on_dstack_i
             (Some (ESub (EVar IDdict_is_permanent_on_dstack_count)
             (EVar IDdict_is_permanent_on_dstack_min_dstack_size)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_i) s) <
             (eval (EVar IDdict_is_permanent_on_dstack_count) s))%Z)),
             14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_i) s) >=
             (eval (EVar IDdict_is_permanent_on_dstack_count) s))%Z)),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,ANone,31%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,22%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDdict_is_permanent_on_dstack_i
             (Some (EAdd (EVar IDdict_is_permanent_on_dstack_i)
             (ENum (1))))),18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDdict_is_permanent_on_dstack_z
             (Some (EAdd (ENum (1))
             (EVar IDdict_is_permanent_on_dstack_z)))),21%positive)::
             (21%positive,AWeaken,11%positive)::
             (22%positive,(AAssign IDdict_is_permanent_on_dstack__tmp
             (Some (ENum (1)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,45%positive)::
             (25%positive,(AAssign IDdict_is_permanent_on_dstack_i
             (Some (ENum (0)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_i) s) <
             (eval (EVar IDdict_is_permanent_on_dstack_min_dstack_size)
             s))%Z)),34%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDdict_is_permanent_on_dstack_i) s) >=
             (eval (EVar IDdict_is_permanent_on_dstack_min_dstack_size)
             s))%Z)),29%positive)::(29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDdict_is_permanent_on_dstack__tmp
             (Some (ENum (0)))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,45%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,42%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDdict_is_permanent_on_dstack_i
             (Some (EAdd (EVar IDdict_is_permanent_on_dstack_i)
             (ENum (1))))),38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDdict_is_permanent_on_dstack_z
             (Some (EAdd (ENum (1))
             (EVar IDdict_is_permanent_on_dstack_z)))),41%positive)::
             (41%positive,AWeaken,28%positive)::
             (42%positive,(AAssign IDdict_is_permanent_on_dstack__tmp
             (Some (ENum (1)))),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::nil
|}.

Definition dict_is_permanent_on_dstack_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 4%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 5%positive => (-1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 6%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 7%positive => (-1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 8%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 9%positive => (-1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 10%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0)%Z
    | 11%positive => (-1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 12%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_count)+ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 13%positive => (1 * (s IDdict_is_permanent_on_dstack_count)+ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 14%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 16%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 18%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 19%positive => (-1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 20%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 21%positive => (-1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) + -1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack__tmp) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDdict_is_permanent_on_dstack__tmp) + 1 <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) + -1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count)+ 1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 26%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0)%Z
    | 27%positive => (-1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 28%positive => (-1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 29%positive => (1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i)+ 1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 30%positive => (-1 * (s IDdict_is_permanent_on_dstack_i)+ 1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0)%Z
    | 31%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0)%Z
    | 32%positive => (-1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack__tmp) <= 0)%Z
    | 33%positive => (-1 * (s IDdict_is_permanent_on_dstack__tmp) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0)%Z
    | 34%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 36%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0)%Z
    | 37%positive => (1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 38%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0)%Z
    | 40%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDdict_is_permanent_on_dstack_i) + 1 <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0)%Z
    | 43%positive => (1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) + -1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack__tmp) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDdict_is_permanent_on_dstack__tmp) + 1 <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) + -1 <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_count) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_i) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack_i)+ -1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDdict_is_permanent_on_dstack_min_dstack_size) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack__tmp) <= 0 /\ -1 * (s IDdict_is_permanent_on_dstack_z) <= 0 /\ 1 * (s IDdict_is_permanent_on_dstack__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition dict_is_permanent_on_dstack_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 2%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 3%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 4%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 5%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 6%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 7%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 8%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 9%positive => ((s IDdict_is_permanent_on_dstack_z)
                     + max0((s IDdict_is_permanent_on_dstack_count)
                            - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 10%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 11%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 12%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 13%positive => ((s IDdict_is_permanent_on_dstack_z))%Q
    | 14%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 15%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0(-1 + (s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 16%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0(-1 + (s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 17%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0(-1 + (s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 18%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 19%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 20%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 21%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 22%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0(-1 + (s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 23%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0(-1 + (s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 24%positive => ((1 # 1) + (s IDdict_is_permanent_on_dstack_z)
                      + max0(-1 + (s IDdict_is_permanent_on_dstack_count)
                             - (s IDdict_is_permanent_on_dstack_i)))%Q
    | 25%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0((s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 26%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0(-(s IDdict_is_permanent_on_dstack_i)
                             + (s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 27%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0(-(s IDdict_is_permanent_on_dstack_i)
                             + (s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 28%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0(-(s IDdict_is_permanent_on_dstack_i)
                             + (s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 29%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0(-(s IDdict_is_permanent_on_dstack_i)
                             + (s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 30%positive => ((s IDdict_is_permanent_on_dstack_z))%Q
    | 31%positive => ((s IDdict_is_permanent_on_dstack_z))%Q
    | 32%positive => ((s IDdict_is_permanent_on_dstack_z))%Q
    | 33%positive => ((s IDdict_is_permanent_on_dstack_z))%Q
    | 34%positive => ((s IDdict_is_permanent_on_dstack_z)
                      + max0(-(s IDdict_is_permanent_on_dstack_i)
                             + (s IDdict_is_permanent_on_dstack_min_dstack_size)))%Q
    | 35%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 36%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 37%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 38%positive => ((1 # 1) - (s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 39%positive => ((1 # 1) - (s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 40%positive => ((1 # 1) - (s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 41%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 42%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 43%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 44%positive => (-(s IDdict_is_permanent_on_dstack_i)
                      + (s IDdict_is_permanent_on_dstack_min_dstack_size)
                      + (s IDdict_is_permanent_on_dstack_z))%Q
    | 45%positive => ((s IDdict_is_permanent_on_dstack_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition dict_is_permanent_on_dstack_hints (p : node) (s : state) := 
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
    | 12%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDdict_is_permanent_on_dstack_count)
                                                            - (s IDdict_is_permanent_on_dstack_i)) (-1
                                                                    + (s IDdict_is_permanent_on_dstack_count)
                                                                    - (s IDdict_is_permanent_on_dstack_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDdict_is_permanent_on_dstack_count)
                                                                 - (s IDdict_is_permanent_on_dstack_i))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement ((s IDdict_is_permanent_on_dstack_count)
                                                     - (s IDdict_is_permanent_on_dstack_i)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDdict_is_permanent_on_dstack_count)
                                            - (s IDdict_is_permanent_on_dstack_i))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDdict_is_permanent_on_dstack_i)
                                                             + (s IDdict_is_permanent_on_dstack_min_dstack_size)) (-1
                                                                    - (s IDdict_is_permanent_on_dstack_i)
                                                                    + (s IDdict_is_permanent_on_dstack_min_dstack_size)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDdict_is_permanent_on_dstack_i)
                                            + (s IDdict_is_permanent_on_dstack_min_dstack_size))]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-(s IDdict_is_permanent_on_dstack_i)
                                                                   + 
                                                                   (s IDdict_is_permanent_on_dstack_min_dstack_size))) (F_check_ge (-
                                                                    (s IDdict_is_permanent_on_dstack_i)
                                                                    + (s IDdict_is_permanent_on_dstack_min_dstack_size)) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDdict_is_permanent_on_dstack_i)
                                                                    + (s IDdict_is_permanent_on_dstack_min_dstack_size)) (0))) (F_max0_ge_0 (-
                                                                    (s IDdict_is_permanent_on_dstack_i)
                                                                    + (s IDdict_is_permanent_on_dstack_min_dstack_size)))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement (-(s IDdict_is_permanent_on_dstack_i)
                                                     + (s IDdict_is_permanent_on_dstack_min_dstack_size)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDdict_is_permanent_on_dstack_i)
                                            + (s IDdict_is_permanent_on_dstack_min_dstack_size));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDdict_is_permanent_on_dstack_i)
                                                                    + (s IDdict_is_permanent_on_dstack_min_dstack_size)) (0))) (F_max0_ge_0 (-
                                                                    (s IDdict_is_permanent_on_dstack_i)
                                                                    + (s IDdict_is_permanent_on_dstack_min_dstack_size)))]
    | 45%positive => []
    | _ => []
  end.


Theorem dict_is_permanent_on_dstack_ai_correct:
  forall s p' s', steps (g_start dict_is_permanent_on_dstack) s (g_edges dict_is_permanent_on_dstack) p' s' -> dict_is_permanent_on_dstack_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem dict_is_permanent_on_dstack_pot_correct:
  forall s p' s',
    steps (g_start dict_is_permanent_on_dstack) s (g_edges dict_is_permanent_on_dstack) p' s' ->
    (dict_is_permanent_on_dstack_pot (g_start dict_is_permanent_on_dstack) s >= dict_is_permanent_on_dstack_pot p' s')%Q.
Proof.
  check_lp dict_is_permanent_on_dstack_ai_correct dict_is_permanent_on_dstack_hints.
Qed.

