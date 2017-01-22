Require Import pasta.Pasta.

Notation IDrandPoolStir_z := 1%positive.
Notation IDrandPoolStir_i := 2%positive.
Notation IDrandPoolStir_randPoolAddPos := 3%positive.
Notation IDrandPoolStir_randPoolGetPos := 4%positive.
Notation IDrandPoolStir_t := 5%positive.
Definition randPoolStir : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDrandPoolStir_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDrandPoolStir_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) < (eval (ENum (96)) s))%Z)),48%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) >= (eval (ENum (96)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDrandPoolStir_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) < (eval (ENum (96)) s))%Z)),41%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) >= (eval (ENum (96)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDrandPoolStir_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) < (eval (ENum (96)) s))%Z)),34%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) >= (eval (ENum (96)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDrandPoolStir_i (Some (ENum (0)))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) < (eval (ENum (96)) s))%Z)),26%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDrandPoolStir_i)
             s) >= (eval (ENum (96)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDrandPoolStir_randPoolAddPos
             (Some (ENum (0)))),23%positive)::
             (23%positive,(AAssign IDrandPoolStir_randPoolGetPos
             (Some (ENum (128)))),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDrandPoolStir_t None),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDrandPoolStir_i
             (Some (EAdd (EVar IDrandPoolStir_i) (ENum (1))))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDrandPoolStir_z (Some (EAdd (ENum (1))
             (EVar IDrandPoolStir_z)))),33%positive)::
             (33%positive,AWeaken,20%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDrandPoolStir_i
             (Some (EAdd (EVar IDrandPoolStir_i) (ENum (4))))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDrandPoolStir_z (Some (EAdd (ENum (1))
             (EVar IDrandPoolStir_z)))),40%positive)::
             (40%positive,AWeaken,15%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDrandPoolStir_i
             (Some (EAdd (EVar IDrandPoolStir_i) (ENum (4))))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDrandPoolStir_z (Some (EAdd (ENum (1))
             (EVar IDrandPoolStir_z)))),47%positive)::
             (47%positive,AWeaken,10%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDrandPoolStir_t None),50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDrandPoolStir_i
             (Some (EAdd (EVar IDrandPoolStir_i) (ENum (1))))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDrandPoolStir_z (Some (EAdd (ENum (1))
             (EVar IDrandPoolStir_z)))),55%positive)::
             (55%positive,AWeaken,5%positive)::nil
|}.

Definition randPoolStir_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 4%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 5%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0)%Z
    | 6%positive => (1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0)%Z
    | 7%positive => (-1 * (s IDrandPoolStir_i) + 96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0)%Z
    | 8%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 9%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 10%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 11%positive => (1 * (s IDrandPoolStir_i) + -99 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0)%Z
    | 12%positive => (-1 * (s IDrandPoolStir_i) + 96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 13%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 14%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 15%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 16%positive => (1 * (s IDrandPoolStir_i) + -99 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0)%Z
    | 17%positive => (-1 * (s IDrandPoolStir_i) + 96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 18%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 19%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 20%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0)%Z
    | 21%positive => (1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0)%Z
    | 22%positive => (-1 * (s IDrandPoolStir_i) + 96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0)%Z
    | 23%positive => (1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0 /\ 1 * (s IDrandPoolStir_randPoolAddPos) <= 0 /\ -1 * (s IDrandPoolStir_randPoolAddPos) <= 0)%Z
    | 24%positive => (-1 * (s IDrandPoolStir_randPoolAddPos) <= 0 /\ 1 * (s IDrandPoolStir_randPoolAddPos) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ 1 * (s IDrandPoolStir_randPoolGetPos) + -128 <= 0 /\ -1 * (s IDrandPoolStir_randPoolGetPos) + 128 <= 0)%Z
    | 25%positive => (-1 * (s IDrandPoolStir_randPoolGetPos) + 128 <= 0 /\ 1 * (s IDrandPoolStir_randPoolGetPos) + -128 <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 96 <= 0 /\ 1 * (s IDrandPoolStir_randPoolAddPos) <= 0 /\ -1 * (s IDrandPoolStir_randPoolAddPos) <= 0)%Z
    | 26%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 27%positive => (1 * (s IDrandPoolStir_i) + -95 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 28%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 29%positive => (1 * (s IDrandPoolStir_i) + -95 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 30%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDrandPoolStir_i) + 1 <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 32%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDrandPoolStir_i) + 1 <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 35%positive => (1 * (s IDrandPoolStir_i) + -95 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 36%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 37%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 38%positive => (1 * (s IDrandPoolStir_i) + -99 <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 39%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 40%positive => (1 * (s IDrandPoolStir_i) + -99 <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ -1 * (s IDrandPoolStir_z) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 42%positive => (1 * (s IDrandPoolStir_i) + -95 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 43%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 44%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 45%positive => (1 * (s IDrandPoolStir_i) + -99 <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 46%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ 1 * (s IDrandPoolStir_i) + -99 <= 0)%Z
    | 47%positive => (1 * (s IDrandPoolStir_i) + -99 <= 0 /\ -1 * (s IDrandPoolStir_i) + 4 <= 0 /\ -1 * (s IDrandPoolStir_z) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 49%positive => (1 * (s IDrandPoolStir_i) + -95 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 50%positive => (-1 * (s IDrandPoolStir_i) <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -95 <= 0)%Z
    | 51%positive => (1 * (s IDrandPoolStir_i) + -95 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0 /\ -1 * (s IDrandPoolStir_i) <= 0)%Z
    | 52%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_i) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDrandPoolStir_i) + 1 <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) <= 0)%Z
    | 54%positive => (-1 * (s IDrandPoolStir_z) <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_i) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDrandPoolStir_i) + 1 <= 0 /\ 1 * (s IDrandPoolStir_i) + -96 <= 0 /\ -1 * (s IDrandPoolStir_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition randPoolStir_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((483 # 2))%Q
    | 2%positive => ((483 # 2) + (s IDrandPoolStir_z))%Q
    | 3%positive => ((291 # 2) + (s IDrandPoolStir_z)
                     + max0(96 - (s IDrandPoolStir_i)))%Q
    | 4%positive => ((291 # 2) + (s IDrandPoolStir_z)
                     + max0(96 - (s IDrandPoolStir_i)))%Q
    | 5%positive => ((291 # 2) + (s IDrandPoolStir_z)
                     + max0(96 - (s IDrandPoolStir_i)))%Q
    | 6%positive => ((291 # 2) + (s IDrandPoolStir_z)
                     + max0(96 - (s IDrandPoolStir_i)))%Q
    | 7%positive => ((291 # 2) + (s IDrandPoolStir_z))%Q
    | 8%positive => ((483 # 4) + (s IDrandPoolStir_z)
                     + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 9%positive => ((483 # 4) + (s IDrandPoolStir_z)
                     + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 10%positive => ((483 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 11%positive => ((483 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 12%positive => ((483 # 4) + (s IDrandPoolStir_z))%Q
    | 13%positive => ((96 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 14%positive => ((96 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 15%positive => ((96 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 16%positive => ((96 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 17%positive => ((96 # 1) + (s IDrandPoolStir_z))%Q
    | 18%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 19%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 20%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 21%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 22%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 23%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 24%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 25%positive => ((s IDrandPoolStir_z))%Q
    | 26%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 27%positive => ((1 # 1) + (s IDrandPoolStir_z)
                      + max0(95 - (s IDrandPoolStir_i)))%Q
    | 28%positive => ((1 # 1) + (s IDrandPoolStir_z)
                      + max0(95 - (s IDrandPoolStir_i)))%Q
    | 29%positive => ((1 # 1) + (s IDrandPoolStir_z)
                      + max0(95 - (s IDrandPoolStir_i)))%Q
    | 30%positive => ((1 # 1) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 31%positive => ((1 # 1) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 32%positive => ((1 # 1) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 33%positive => ((s IDrandPoolStir_z) + max0(96 - (s IDrandPoolStir_i)))%Q
    | 34%positive => ((96 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 35%positive => ((97 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(95 - (s IDrandPoolStir_i)))%Q
    | 36%positive => ((97 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(95 - (s IDrandPoolStir_i)))%Q
    | 37%positive => ((97 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 38%positive => ((97 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 39%positive => ((97 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 40%positive => ((96 # 1) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 41%positive => ((483 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 42%positive => ((487 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(95 - (s IDrandPoolStir_i)))%Q
    | 43%positive => ((487 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(95 - (s IDrandPoolStir_i)))%Q
    | 44%positive => ((487 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 45%positive => ((487 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 46%positive => ((487 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 47%positive => ((483 # 4) + (s IDrandPoolStir_z)
                      + (1 # 4) * max0(99 - (s IDrandPoolStir_i)))%Q
    | 48%positive => ((291 # 2) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 49%positive => ((293 # 2) + (s IDrandPoolStir_z)
                      + max0(95 - (s IDrandPoolStir_i)))%Q
    | 50%positive => ((293 # 2) + (s IDrandPoolStir_z)
                      + max0(95 - (s IDrandPoolStir_i)))%Q
    | 51%positive => ((293 # 2) + (s IDrandPoolStir_z)
                      + max0(95 - (s IDrandPoolStir_i)))%Q
    | 52%positive => ((293 # 2) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 53%positive => ((293 # 2) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 54%positive => ((293 # 2) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | 55%positive => ((291 # 2) + (s IDrandPoolStir_z)
                      + max0(96 - (s IDrandPoolStir_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition randPoolStir_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (96
                                                            - (s IDrandPoolStir_i)) (92
                                                                    - (s IDrandPoolStir_i)));
                     (*-1 0*) F_max0_ge_0 (92 - (s IDrandPoolStir_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (99
                                                                - (s IDrandPoolStir_i)) (95
                                                                    - (s IDrandPoolStir_i)));
                      (*-0.25 0*) F_max0_ge_0 (95 - (s IDrandPoolStir_i))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (99
                                                                - (s IDrandPoolStir_i)) (98
                                                                    - (s IDrandPoolStir_i)));
                      (*-0.25 0*) F_max0_ge_0 (98 - (s IDrandPoolStir_i))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (96
                                                                 - (s IDrandPoolStir_i))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => [(*-1 1e-12*) F_max0_pre_decrement (96
                                                         - (s IDrandPoolStir_i)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*0 0.25*) F_max0_pre_decrement (99
                                                       - (s IDrandPoolStir_i)) (4)]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-0.25 0*) F_max0_pre_decrement (99
                                                        - (s IDrandPoolStir_i)) (4)]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_max0_pre_decrement (96
                                                     - (s IDrandPoolStir_i)) (1)]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | _ => []
  end.


Theorem randPoolStir_ai_correct:
  forall s p' s', steps (g_start randPoolStir) s (g_edges randPoolStir) p' s' -> randPoolStir_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem randPoolStir_pot_correct:
  forall s p' s',
    steps (g_start randPoolStir) s (g_edges randPoolStir) p' s' ->
    (randPoolStir_pot (g_start randPoolStir) s >= randPoolStir_pot p' s')%Q.
Proof.
  check_lp randPoolStir_ai_correct randPoolStir_hints.
Qed.

