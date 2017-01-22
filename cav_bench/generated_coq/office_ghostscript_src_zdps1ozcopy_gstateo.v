Require Import pasta.Pasta.

Notation IDzcopy_gstate_z := 1%positive.
Notation IDzcopy_gstate__tmp := 2%positive.
Notation IDzcopy_gstate_code := 3%positive.
Notation IDzcopy_gstate_i := 4%positive.
Notation IDzcopy_gstate_i2 := 5%positive.
Notation IDzcopy_gstate_op := 6%positive.
Definition zcopy_gstate : graph := {|
  g_start := 1%positive;
  g_end := 73%positive;
  g_edges := (1%positive,(AAssign IDzcopy_gstate_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,4%positive)::(3%positive,ANone,6%positive)::
             (4%positive,AWeaken,5%positive)::(5%positive,ANone,9%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDzcopy_gstate__tmp None),7%positive)::
             (7%positive,ANone,8%positive)::
             (8%positive,AWeaken,73%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,13%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,16%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDzcopy_gstate__tmp None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,73%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,21%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDzcopy_gstate__tmp (Some (ENum (-7)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,73%positive)::
             (21%positive,(AAssign IDzcopy_gstate_code None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDzcopy_gstate_code)
             s) < (eval (ENum (0)) s))%Z)),69%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDzcopy_gstate_code)
             s) >= (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDzcopy_gstate_code None),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDzcopy_gstate_code)
             s) < (eval (ENum (0)) s))%Z)),65%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDzcopy_gstate_code)
             s) >= (eval (ENum (0)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDzcopy_gstate_i (Some (ENum (25)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,34%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,35%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDzcopy_gstate_i
             (Some (EAdd (EVar IDzcopy_gstate_i) (ENum (-1))))),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcopy_gstate_i) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),61%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcopy_gstate_i) (ENum (-1))) s) =
             (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDzcopy_gstate_code None),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDzcopy_gstate_code)
             s) < (eval (ENum (0)) s))%Z)),57%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDzcopy_gstate_code)
             s) >= (eval (ENum (0)) s))%Z)),43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AAssign IDzcopy_gstate_i2 (Some (ENum (25)))),
             45%positive)::(45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDzcopy_gstate_i2
             (Some (EAdd (EVar IDzcopy_gstate_i2) (ENum (-1))))),48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcopy_gstate_i2) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),54%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDzcopy_gstate_i2) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AAssign IDzcopy_gstate__tmp (Some (ENum (0)))),
             52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,AWeaken,73%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDzcopy_gstate_z (Some (EAdd (ENum (1))
             (EVar IDzcopy_gstate_z)))),46%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,(AAssign IDzcopy_gstate__tmp
             (Some (EVar IDzcopy_gstate_code))),59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,AWeaken,73%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,(AAssign IDzcopy_gstate_z (Some (EAdd (ENum (1))
             (EVar IDzcopy_gstate_z)))),64%positive)::
             (64%positive,AWeaken,32%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AAssign IDzcopy_gstate__tmp
             (Some (EVar IDzcopy_gstate_code))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,73%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,(AAssign IDzcopy_gstate__tmp
             (Some (EVar IDzcopy_gstate_code))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,73%positive)::nil
|}.

Definition zcopy_gstate_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 4%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 6%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 8%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 9%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 10%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 11%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 12%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 13%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 14%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 15%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 16%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 17%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 18%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 19%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate__tmp) + 7 <= 0 /\ -1 * (s IDzcopy_gstate__tmp) + -7 <= 0)%Z
    | 20%positive => (-1 * (s IDzcopy_gstate__tmp) + -7 <= 0 /\ 1 * (s IDzcopy_gstate__tmp) + 7 <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 21%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 22%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 23%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 24%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0)%Z
    | 25%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 26%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 27%positive => (1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 28%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0)%Z
    | 29%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 30%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 25 <= 0)%Z
    | 31%positive => (-1 * (s IDzcopy_gstate_i) + 25 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 32%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0)%Z
    | 33%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 34%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 35%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0)%Z
    | 36%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 37%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -24 <= 0)%Z
    | 38%positive => (1 * (s IDzcopy_gstate_i) + -24 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 39%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 41%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 43%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0)%Z
    | 44%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 45%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_i2) + 25 <= 0)%Z
    | 46%positive => (1 * (s IDzcopy_gstate_i2) + -25 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0)%Z
    | 47%positive => (-1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -25 <= 0)%Z
    | 48%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -24 <= 0)%Z
    | 49%positive => (1 * (s IDzcopy_gstate_i2) + -24 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 50%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i2) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDzcopy_gstate_i2) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 52%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i2) + 1 <= 0 /\ 1 * (s IDzcopy_gstate__tmp) <= 0 /\ -1 * (s IDzcopy_gstate__tmp) <= 0)%Z
    | 53%positive => (-1 * (s IDzcopy_gstate__tmp) <= 0 /\ 1 * (s IDzcopy_gstate__tmp) <= 0 /\ -1 * (s IDzcopy_gstate_i2) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 54%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -24 <= 0)%Z
    | 55%positive => (1 * (s IDzcopy_gstate_i2) + -24 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 56%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i2) + -24 <= 0)%Z
    | 57%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0)%Z
    | 58%positive => (1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 59%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate__tmp) + 1 <= 0)%Z
    | 60%positive => (1 * (s IDzcopy_gstate__tmp) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ -1 * (s IDzcopy_gstate_i) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_i) + -1 <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 61%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -24 <= 0)%Z
    | 62%positive => (1 * (s IDzcopy_gstate_i) + -24 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 63%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ 1 * (s IDzcopy_gstate_i) + -24 <= 0)%Z
    | 64%positive => (1 * (s IDzcopy_gstate_i) + -24 <= 0 /\ -1 * (s IDzcopy_gstate_code) <= 0 /\ -1 * (s IDzcopy_gstate_z) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0)%Z
    | 66%positive => (1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 67%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate__tmp) + 1 <= 0)%Z
    | 68%positive => (1 * (s IDzcopy_gstate__tmp) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 69%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0)%Z
    | 70%positive => (1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 71%positive => (-1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate__tmp) + 1 <= 0)%Z
    | 72%positive => (1 * (s IDzcopy_gstate__tmp) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_code) + 1 <= 0 /\ 1 * (s IDzcopy_gstate_z) <= 0 /\ -1 * (s IDzcopy_gstate_z) <= 0)%Z
    | 73%positive => (-1 * (s IDzcopy_gstate_z) <= 0)%Z
    | _ => False
  end.

Definition zcopy_gstate_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((46 # 1))%Q
    | 2%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 3%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 4%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 5%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 6%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 7%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 8%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 9%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 10%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 11%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 12%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 13%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 14%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 15%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 16%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 17%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 18%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 19%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 20%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 21%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 22%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 23%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 24%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 25%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 26%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 27%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 28%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 29%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 30%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 31%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 32%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 33%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 34%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 35%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 36%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 37%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 38%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 39%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 40%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 41%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 42%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 43%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 44%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 45%positive => (-(2 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 46%positive => (-(2 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 47%positive => (-(2 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 48%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 49%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 50%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 51%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 52%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 53%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 54%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 55%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 56%positive => (-(1 # 1) + (s IDzcopy_gstate_i2) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 57%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 58%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 59%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 60%positive => ((23 # 1) + (s IDzcopy_gstate_z)
                      + max0(-1 + (s IDzcopy_gstate_i)))%Q
    | 61%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 62%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 63%positive => ((22 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 64%positive => ((21 # 1) + (s IDzcopy_gstate_i) + (s IDzcopy_gstate_z))%Q
    | 65%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 66%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 67%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 68%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 69%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 70%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 71%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 72%positive => ((46 # 1) + (s IDzcopy_gstate_z))%Q
    | 73%positive => ((s IDzcopy_gstate_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zcopy_gstate_hints (p : node) (s : state) := 
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
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-46 0*) F_one]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*0 46*) F_one]
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
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzcopy_gstate_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzcopy_gstate_i)))]
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
    | 53%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzcopy_gstate_i2))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDzcopy_gstate_i2)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDzcopy_gstate_i2)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzcopy_gstate_i))) (F_check_ge (0) (0))]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => [(*-23 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDzcopy_gstate_i))) (F_check_ge (0) (0))]
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => [(*-46 0*) F_one]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => [(*-46 0*) F_one]
    | 73%positive => []
    | _ => []
  end.


Theorem zcopy_gstate_ai_correct:
  forall s p' s', steps (g_start zcopy_gstate) s (g_edges zcopy_gstate) p' s' -> zcopy_gstate_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zcopy_gstate_pot_correct:
  forall s p' s',
    steps (g_start zcopy_gstate) s (g_edges zcopy_gstate) p' s' ->
    (zcopy_gstate_pot (g_start zcopy_gstate) s >= zcopy_gstate_pot p' s')%Q.
Proof.
  check_lp zcopy_gstate_ai_correct zcopy_gstate_hints.
Qed.

