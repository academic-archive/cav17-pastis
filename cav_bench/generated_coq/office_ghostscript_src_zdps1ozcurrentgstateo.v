Require Import pasta.Pasta.

Notation IDzcurrentgstate_z := 1%positive.
Notation IDzcurrentgstate__tmp := 2%positive.
Notation IDzcurrentgstate_code := 3%positive.
Notation IDzcurrentgstate_i := 4%positive.
Notation IDzcurrentgstate_i2 := 5%positive.
Notation IDzcurrentgstate_op := 6%positive.
Definition zcurrentgstate : graph := {|
  g_start := 1%positive;
  g_end := 66%positive;
  g_edges := (1%positive,(AAssign IDzcurrentgstate_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,4%positive)::(3%positive,ANone,6%positive)::
             (4%positive,AWeaken,5%positive)::(5%positive,ANone,9%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDzcurrentgstate__tmp None),7%positive)::
             (7%positive,ANone,8%positive)::
             (8%positive,AWeaken,66%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,14%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDzcurrentgstate__tmp (Some (ENum (-7)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,66%positive)::
             (14%positive,(AAssign IDzcurrentgstate_code None),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrentgstate_code) s) <
             (eval (ENum (0)) s))%Z)),62%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrentgstate_code) s) >=
             (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDzcurrentgstate_code None),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrentgstate_code) s) <
             (eval (ENum (0)) s))%Z)),58%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrentgstate_code) s) >=
             (eval (ENum (0)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDzcurrentgstate_i (Some (ENum (25)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,27%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,28%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDzcurrentgstate_i
             (Some (EAdd (EVar IDzcurrentgstate_i) (ENum (-1))))),
             30%positive)::(30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcurrentgstate_i) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),54%positive)::
             (31%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcurrentgstate_i) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDzcurrentgstate_code None),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrentgstate_code) s) <
             (eval (ENum (0)) s))%Z)),50%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDzcurrentgstate_code) s) >=
             (eval (ENum (0)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDzcurrentgstate_i2 (Some (ENum (25)))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDzcurrentgstate_i2
             (Some (EAdd (EVar IDzcurrentgstate_i2) (ENum (-1))))),
             41%positive)::(41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcurrentgstate_i2) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),47%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcurrentgstate_i2) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AAssign IDzcurrentgstate__tmp (Some (ENum (0)))),
             45%positive)::(45%positive,ANone,46%positive)::
             (46%positive,AWeaken,66%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDzcurrentgstate_z (Some (EAdd (ENum (1))
             (EVar IDzcurrentgstate_z)))),39%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AAssign IDzcurrentgstate__tmp
             (Some (EVar IDzcurrentgstate_code))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,66%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDzcurrentgstate_z (Some (EAdd (ENum (1))
             (EVar IDzcurrentgstate_z)))),57%positive)::
             (57%positive,AWeaken,25%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AAssign IDzcurrentgstate__tmp
             (Some (EVar IDzcurrentgstate_code))),60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,66%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AAssign IDzcurrentgstate__tmp
             (Some (EVar IDzcurrentgstate_code))),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,AWeaken,66%positive)::nil
|}.

Definition zcurrentgstate_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 4%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 6%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 8%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 9%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 10%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 11%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 12%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate__tmp) + 7 <= 0 /\ -1 * (s IDzcurrentgstate__tmp) + -7 <= 0)%Z
    | 13%positive => (-1 * (s IDzcurrentgstate__tmp) + -7 <= 0 /\ 1 * (s IDzcurrentgstate__tmp) + 7 <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 14%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 15%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 16%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 17%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0)%Z
    | 18%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 19%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 20%positive => (1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 21%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0)%Z
    | 22%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 23%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 25 <= 0)%Z
    | 24%positive => (-1 * (s IDzcurrentgstate_i) + 25 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 25%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0)%Z
    | 26%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 27%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 28%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0)%Z
    | 29%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 30%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -24 <= 0)%Z
    | 31%positive => (1 * (s IDzcurrentgstate_i) + -24 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 32%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 34%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 36%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0)%Z
    | 37%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 38%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_i2) + 25 <= 0)%Z
    | 39%positive => (1 * (s IDzcurrentgstate_i2) + -25 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0)%Z
    | 40%positive => (-1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -25 <= 0)%Z
    | 41%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -24 <= 0)%Z
    | 42%positive => (1 * (s IDzcurrentgstate_i2) + -24 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 43%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i2) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDzcurrentgstate_i2) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 45%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i2) + 1 <= 0 /\ 1 * (s IDzcurrentgstate__tmp) <= 0 /\ -1 * (s IDzcurrentgstate__tmp) <= 0)%Z
    | 46%positive => (-1 * (s IDzcurrentgstate__tmp) <= 0 /\ 1 * (s IDzcurrentgstate__tmp) <= 0 /\ -1 * (s IDzcurrentgstate_i2) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 47%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -24 <= 0)%Z
    | 48%positive => (1 * (s IDzcurrentgstate_i2) + -24 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 49%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i2) + -24 <= 0)%Z
    | 50%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 52%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate__tmp) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDzcurrentgstate__tmp) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ -1 * (s IDzcurrentgstate_i) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_i) + -1 <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 54%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -24 <= 0)%Z
    | 55%positive => (1 * (s IDzcurrentgstate_i) + -24 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 56%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ 1 * (s IDzcurrentgstate_i) + -24 <= 0)%Z
    | 57%positive => (1 * (s IDzcurrentgstate_i) + -24 <= 0 /\ -1 * (s IDzcurrentgstate_code) <= 0 /\ -1 * (s IDzcurrentgstate_z) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0)%Z
    | 59%positive => (1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 60%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate__tmp) + 1 <= 0)%Z
    | 61%positive => (1 * (s IDzcurrentgstate__tmp) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 62%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0)%Z
    | 63%positive => (1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 64%positive => (-1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate__tmp) + 1 <= 0)%Z
    | 65%positive => (1 * (s IDzcurrentgstate__tmp) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_code) + 1 <= 0 /\ 1 * (s IDzcurrentgstate_z) <= 0 /\ -1 * (s IDzcurrentgstate_z) <= 0)%Z
    | 66%positive => (-1 * (s IDzcurrentgstate_z) <= 0)%Z
    | _ => False
  end.

Definition zcurrentgstate_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((46 # 1))%Q
    | 2%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 3%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 4%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 5%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 6%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 7%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 8%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 9%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 10%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 11%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 12%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 13%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 14%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 15%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 16%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 17%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 18%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 19%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 20%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 21%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 22%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 23%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 24%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 25%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 26%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 27%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 28%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 29%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 30%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 31%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 32%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 33%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 34%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 35%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 36%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 37%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 38%positive => (-(2 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 39%positive => (-(2 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 40%positive => (-(2 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 41%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 42%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 43%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 44%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 45%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 46%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 47%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 48%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 49%positive => (-(1 # 1) + (s IDzcurrentgstate_i2)
                      + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 50%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 51%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 52%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 53%positive => ((23 # 1) + (s IDzcurrentgstate_z)
                      + max0(-1 + (s IDzcurrentgstate_i)))%Q
    | 54%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 55%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 56%positive => ((22 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 57%positive => ((21 # 1) + (s IDzcurrentgstate_i)
                      + (s IDzcurrentgstate_z))%Q
    | 58%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 59%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 60%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 61%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 62%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 63%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 64%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 65%positive => ((46 # 1) + (s IDzcurrentgstate_z))%Q
    | 66%positive => ((s IDzcurrentgstate_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zcurrentgstate_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-46 0*) F_one]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*0 46*) F_one]
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
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzcurrentgstate_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzcurrentgstate_i)))]
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
    | 46%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzcurrentgstate_i2))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzcurrentgstate_i2)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzcurrentgstate_i2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzcurrentgstate_i))) (F_check_ge (0) (0))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-23 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzcurrentgstate_i))) (F_check_ge (0) (0))]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*-46 0*) F_one]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => [(*-46 0*) F_one]
    | 66%positive => []
    | _ => []
  end.


Theorem zcurrentgstate_ai_correct:
  forall s p' s', steps (g_start zcurrentgstate) s (g_edges zcurrentgstate) p' s' -> zcurrentgstate_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zcurrentgstate_pot_correct:
  forall s p' s',
    steps (g_start zcurrentgstate) s (g_edges zcurrentgstate) p' s' ->
    (zcurrentgstate_pot (g_start zcurrentgstate) s >= zcurrentgstate_pot p' s')%Q.
Proof.
  check_lp zcurrentgstate_ai_correct zcurrentgstate_hints.
Qed.

