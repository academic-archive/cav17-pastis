Require Import pasta.Pasta.

Notation IDzsetcolorscreen_z := 1%positive.
Notation IDzsetcolorscreen__tmp := 2%positive.
Notation IDzsetcolorscreen_code := 3%positive.
Notation IDzsetcolorscreen_code1 := 4%positive.
Notation IDzsetcolorscreen_es_code_ := 5%positive.
Notation IDzsetcolorscreen_i := 6%positive.
Notation IDzsetcolorscreen_space := 7%positive.
Notation IDzsetcolorscreen_op := 8%positive.
Definition zsetcolorscreen : graph := {|
  g_start := 1%positive;
  g_end := 92%positive;
  g_edges := (1%positive,(AAssign IDzsetcolorscreen_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDzsetcolorscreen_code (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDzsetcolorscreen_space (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDzsetcolorscreen_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDzsetcolorscreen_i)
             s) < (eval (ENum (4)) s))%Z)),73%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDzsetcolorscreen_i)
             s) >= (eval (ENum (4)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::(9%positive,ANone,15%positive)::
             (10%positive,(AAssign IDzsetcolorscreen_es_code_ None),
             11%positive)::(11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_es_code_) s) <
             (eval (ENum (0)) s))%Z)),69%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_es_code_) s) >=
             (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,19%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,20%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,25%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,26%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,34%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,34%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDzsetcolorscreen_code None),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,37%positive)::
             (34%positive,(AAssign IDzsetcolorscreen_code
             (Some (ENum (-25)))),35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code) s) >=
             (eval (ENum (0)) s))%Z)),39%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code) s) <
             (eval (ENum (0)) s))%Z)),38%positive)::
             (38%positive,AWeaken,60%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDzsetcolorscreen_i (Some (ENum (0)))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard (fun s => ((eval (EVar IDzsetcolorscreen_i)
             s) < (eval (ENum (4)) s))%Z)),45%positive)::
             (43%positive,(AGuard (fun s => ((eval (EVar IDzsetcolorscreen_i)
             s) >= (eval (ENum (4)) s))%Z)),44%positive)::
             (44%positive,AWeaken,58%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AAssign IDzsetcolorscreen_code None),47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code) s) <
             (eval (ENum (0)) s))%Z)),56%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code) s) >=
             (eval (ENum (0)) s))%Z)),49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDzsetcolorscreen_i
             (Some (EAdd (EVar IDzsetcolorscreen_i) (ENum (1))))),
             52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDzsetcolorscreen_z (Some (EAdd (ENum (1))
             (EVar IDzsetcolorscreen_z)))),55%positive)::
             (55%positive,AWeaken,43%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code) s) <
             (eval (ENum (0)) s))%Z)),65%positive)::
             (60%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code) s) >=
             (eval (ENum (0)) s))%Z)),61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AAssign IDzsetcolorscreen__tmp (Some (ENum (5)))),
             63%positive)::(63%positive,ANone,64%positive)::
             (64%positive,AWeaken,92%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AAssign IDzsetcolorscreen__tmp
             (Some (EVar IDzsetcolorscreen_code))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,92%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,(AAssign IDzsetcolorscreen__tmp
             (Some (EVar IDzsetcolorscreen_es_code_))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,92%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,(AAssign IDzsetcolorscreen_code1 None),75%positive)::
             (75%positive,AWeaken,76%positive)::
             (76%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code1) s) <
             (eval (ENum (0)) s))%Z)),88%positive)::
             (76%positive,(AGuard
             (fun s => ((eval (EVar IDzsetcolorscreen_code1) s) >=
             (eval (ENum (0)) s))%Z)),77%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,ANone,80%positive)::
             (78%positive,ANone,79%positive)::
             (79%positive,ANone,81%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,(AAssign IDzsetcolorscreen_space None),82%positive)::
             (82%positive,ANone,83%positive)::
             (83%positive,(AAssign IDzsetcolorscreen_i
             (Some (EAdd (EVar IDzsetcolorscreen_i) (ENum (1))))),
             84%positive)::(84%positive,ANone,85%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,(AAssign IDzsetcolorscreen_z (Some (EAdd (ENum (1))
             (EVar IDzsetcolorscreen_z)))),87%positive)::
             (87%positive,AWeaken,7%positive)::
             (88%positive,AWeaken,89%positive)::
             (89%positive,(AAssign IDzsetcolorscreen__tmp
             (Some (EVar IDzsetcolorscreen_code1))),90%positive)::
             (90%positive,ANone,91%positive)::
             (91%positive,AWeaken,92%positive)::nil
|}.

Definition zsetcolorscreen_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 4%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_space) <= 0 /\ -1 * (s IDzsetcolorscreen_space) <= 0)%Z
    | 5%positive => (-1 * (s IDzsetcolorscreen_space) <= 0 /\ 1 * (s IDzsetcolorscreen_space) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0)%Z
    | 6%positive => (-1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_space) <= 0 /\ -1 * (s IDzsetcolorscreen_space) <= 0)%Z
    | 7%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 8%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 9%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 10%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 11%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_es_code_) <= 0)%Z
    | 14%positive => (-1 * (s IDzsetcolorscreen_es_code_) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 15%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 17%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 19%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 20%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 21%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 22%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 23%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 24%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 25%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 26%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 27%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 28%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 29%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 30%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 31%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 32%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 33%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 34%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 35%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 25 <= 0 /\ -1 * (s IDzsetcolorscreen_code) + -25 <= 0)%Z
    | 36%positive => (-1 * (s IDzsetcolorscreen_code) + -25 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 25 <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 37%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 38%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 40%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 41%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0)%Z
    | 42%positive => (-1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 43%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 44%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 45%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0)%Z
    | 46%positive => (1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 47%positive => (-1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0)%Z
    | 48%positive => (1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0)%Z
    | 49%positive => (-1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 50%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0)%Z
    | 51%positive => (-1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 52%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 53%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 1 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 54%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 55%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 1 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 1 <= 0)%Z
    | 57%positive => (1 * (s IDzsetcolorscreen_code) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0)%Z
    | 58%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 59%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0)%Z
    | 60%positive => (1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 61%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 62%positive => (-1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 63%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen__tmp) + -5 <= 0 /\ -1 * (s IDzsetcolorscreen__tmp) + 5 <= 0)%Z
    | 64%positive => (-1 * (s IDzsetcolorscreen__tmp) + 5 <= 0 /\ 1 * (s IDzsetcolorscreen__tmp) + -5 <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 65%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 1 <= 0)%Z
    | 66%positive => (1 * (s IDzsetcolorscreen_code) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 67%positive => (-1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen__tmp) + 1 <= 0)%Z
    | 68%positive => (1 * (s IDzsetcolorscreen__tmp) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_code) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | 69%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_es_code_) + 1 <= 0)%Z
    | 70%positive => (1 * (s IDzsetcolorscreen_es_code_) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 71%positive => (-1 * (s IDzsetcolorscreen_i) + 4 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_es_code_) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen__tmp) + 1 <= 0)%Z
    | 72%positive => (1 * (s IDzsetcolorscreen__tmp) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_es_code_) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 4 <= 0)%Z
    | 73%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0)%Z
    | 74%positive => (1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 75%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0)%Z
    | 76%positive => (1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 77%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0)%Z
    | 78%positive => (-1 * (s IDzsetcolorscreen_code1) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 79%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0)%Z
    | 80%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0)%Z
    | 81%positive => (-1 * (s IDzsetcolorscreen_code1) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 82%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0)%Z
    | 83%positive => (-1 * (s IDzsetcolorscreen_code1) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 84%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 1 <= 0)%Z
    | 85%positive => (-1 * (s IDzsetcolorscreen_i) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 86%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) + 1 <= 0)%Z
    | 87%positive => (-1 * (s IDzsetcolorscreen_i) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_code1) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_z) + 1 <= 0)%Z
    | 88%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ 1 * (s IDzsetcolorscreen_code1) + 1 <= 0)%Z
    | 89%positive => (1 * (s IDzsetcolorscreen_code1) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 90%positive => (1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ 1 * (s IDzsetcolorscreen_code1) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen__tmp) + 1 <= 0)%Z
    | 91%positive => (1 * (s IDzsetcolorscreen__tmp) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_code1) + 1 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -3 <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_code) <= 0 /\ 1 * (s IDzsetcolorscreen_code) <= 0)%Z
    | 92%positive => (1 * (s IDzsetcolorscreen__tmp) + -5 <= 0 /\ 1 * (s IDzsetcolorscreen_i) + -4 <= 0 /\ -1 * (s IDzsetcolorscreen_i) <= 0 /\ -1 * (s IDzsetcolorscreen_z) <= 0)%Z
    | _ => False
  end.

Definition zsetcolorscreen_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDzsetcolorscreen_z))%Q
    | 3%positive => ((8 # 1) + (s IDzsetcolorscreen_z))%Q
    | 4%positive => ((8 # 1) + (s IDzsetcolorscreen_z))%Q
    | 5%positive => ((s IDzsetcolorscreen_z)
                     + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                     + max0((s IDzsetcolorscreen_i)))%Q
    | 6%positive => ((s IDzsetcolorscreen_z)
                     + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                     + max0((s IDzsetcolorscreen_i)))%Q
    | 7%positive => ((s IDzsetcolorscreen_z)
                     + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                     + max0((s IDzsetcolorscreen_i)))%Q
    | 8%positive => ((s IDzsetcolorscreen_z)
                     + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                     + max0((s IDzsetcolorscreen_i)))%Q
    | 9%positive => ((s IDzsetcolorscreen_z)
                     + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                     + max0((s IDzsetcolorscreen_i)))%Q
    | 10%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 11%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 12%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 13%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 14%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 15%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 16%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 17%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 18%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 19%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 20%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 21%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 22%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 23%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 24%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 25%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 26%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 27%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 28%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 29%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 30%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 31%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 32%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 33%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 34%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 35%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 36%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 37%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 38%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 39%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 40%positive => ((4 # 1) + (s IDzsetcolorscreen_z))%Q
    | 41%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 42%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 43%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 44%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 45%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 46%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 47%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 48%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 49%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 50%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 51%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 52%positive => ((5 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 53%positive => ((5 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 54%positive => ((5 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 55%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 56%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 57%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 58%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 59%positive => ((4 # 1) - (s IDzsetcolorscreen_i)
                      + (s IDzsetcolorscreen_z))%Q
    | 60%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 61%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 62%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 63%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 64%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 65%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 66%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 67%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 68%positive => ((s IDzsetcolorscreen_z)
                      + max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 69%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 70%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 71%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 72%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 73%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 74%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 75%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 76%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 77%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 78%positive => ((2 # 1) + (2 # 1) * max0(3 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 79%positive => ((2 # 1) + (2 # 1) * max0(3 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 80%positive => ((2 # 1) + (2 # 1) * max0(3 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 81%positive => ((2 # 1) + (2 # 1) * max0(3 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 82%positive => ((2 # 1) + (2 # 1) * max0(3 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 83%positive => ((2 # 1) + (2 # 1) * max0(3 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 84%positive => ((2 # 1) + max0(-1 + (s IDzsetcolorscreen_i))
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 85%positive => ((2 # 1) + max0(-1 + (s IDzsetcolorscreen_i))
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 86%positive => ((2 # 1) + max0(-1 + (s IDzsetcolorscreen_i))
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_z)))%Q
    | 87%positive => ((2 # 1) + max0(-1 + (s IDzsetcolorscreen_i))
                      + max0(-1 + (s IDzsetcolorscreen_z))
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i)))%Q
    | 88%positive => ((s IDzsetcolorscreen_z)
                      + (2 # 1) * max0(4 - (s IDzsetcolorscreen_i))
                      + max0((s IDzsetcolorscreen_i)))%Q
    | 89%positive => ((s IDzsetcolorscreen_z) + max0((s IDzsetcolorscreen_i)))%Q
    | 90%positive => ((s IDzsetcolorscreen_z) + max0((s IDzsetcolorscreen_i)))%Q
    | 91%positive => ((s IDzsetcolorscreen_z) + max0((s IDzsetcolorscreen_i)))%Q
    | 92%positive => ((s IDzsetcolorscreen_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zsetcolorscreen_hints (p : node) (s : state) := 
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
    | 38%positive => [(*-1 0*) F_max0_ge_0 (4 - (s IDzsetcolorscreen_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzsetcolorscreen_i))) (F_check_ge (0) (0))]
    | 39%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzsetcolorscreen_i)) (3
                                                                    - (s IDzsetcolorscreen_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDzsetcolorscreen_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzsetcolorscreen_i))) (F_check_ge ((s IDzsetcolorscreen_i)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                  - (s IDzsetcolorscreen_i))) (F_check_ge (4
                                                                    - (s IDzsetcolorscreen_i)) (0))]
    | 40%positive => []
    | 41%positive => []
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
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDzsetcolorscreen_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDzsetcolorscreen_i)))]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzsetcolorscreen_i)) (3
                                                                    - (s IDzsetcolorscreen_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDzsetcolorscreen_i))]
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzsetcolorscreen_i)) (3
                                                                    - (s IDzsetcolorscreen_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDzsetcolorscreen_i))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => [(*-2 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzsetcolorscreen_i)) (3
                                                                    - (s IDzsetcolorscreen_i)));
                      (*-2 0*) F_max0_ge_0 (3 - (s IDzsetcolorscreen_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzsetcolorscreen_i))) (F_check_ge (0) (0))]
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => [(*-2 0*) F_max0_pre_decrement (4
                                                     - (s IDzsetcolorscreen_i)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzsetcolorscreen_z)) (0))) (F_max0_ge_0 ((s IDzsetcolorscreen_z)))]
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzsetcolorscreen_i)) (0))) (F_max0_ge_0 ((s IDzsetcolorscreen_i)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDzsetcolorscreen_z))) (F_check_ge (-1
                                                                    + (s IDzsetcolorscreen_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDzsetcolorscreen_i))) (F_check_ge (-1
                                                                    + (s IDzsetcolorscreen_i)) (0))]
    | 88%positive => [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                 - (s IDzsetcolorscreen_i))) (F_check_ge (0) (0))]
    | 89%positive => []
    | 90%positive => []
    | 91%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDzsetcolorscreen_i))) (F_check_ge (0) (0))]
    | 92%positive => []
    | _ => []
  end.


Theorem zsetcolorscreen_ai_correct:
  forall s p' s', steps (g_start zsetcolorscreen) s (g_edges zsetcolorscreen) p' s' -> zsetcolorscreen_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zsetcolorscreen_pot_correct:
  forall s p' s',
    steps (g_start zsetcolorscreen) s (g_edges zsetcolorscreen) p' s' ->
    (zsetcolorscreen_pot (g_start zsetcolorscreen) s >= zsetcolorscreen_pot p' s')%Q.
Proof.
  check_lp zsetcolorscreen_ai_correct zsetcolorscreen_hints.
Qed.

