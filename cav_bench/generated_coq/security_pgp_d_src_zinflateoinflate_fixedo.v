Require Import pasta.Pasta.

Notation IDinflate_fixed_z := 1%positive.
Notation IDinflate_fixed__tmp := 2%positive.
Notation IDinflate_fixed_i := 3%positive.
Definition inflate_fixed : graph := {|
  g_start := 1%positive;
  g_end := 51%positive;
  g_edges := (1%positive,(AAssign IDinflate_fixed_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDinflate_fixed_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) < (eval (ENum (144)) s))%Z)),73%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) >= (eval (ENum (144)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) < (eval (ENum (256)) s))%Z)),66%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) >= (eval (ENum (256)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) < (eval (ENum (280)) s))%Z)),59%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) >= (eval (ENum (280)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) < (eval (ENum (288)) s))%Z)),52%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) >= (eval (ENum (288)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDinflate_fixed_i None),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,48%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDinflate_fixed_i (Some (ENum (0)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) < (eval (ENum (30)) s))%Z)),41%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDinflate_fixed_i)
             s) >= (eval (ENum (30)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDinflate_fixed_i None),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,38%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,35%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDinflate_fixed__tmp (Some (ENum (0)))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,AWeaken,51%positive)::
             (35%positive,(AAssign IDinflate_fixed__tmp (Some (ENum (1)))),
             36%positive)::(36%positive,ANone,37%positive)::
             (37%positive,AWeaken,51%positive)::
             (38%positive,(AAssign IDinflate_fixed__tmp
             (Some (EVar IDinflate_fixed_i))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,51%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDinflate_fixed_i
             (Some (EAdd (EVar IDinflate_fixed_i) (ENum (1))))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDinflate_fixed_z (Some (EAdd (ENum (1))
             (EVar IDinflate_fixed_z)))),47%positive)::
             (47%positive,AWeaken,25%positive)::
             (48%positive,(AAssign IDinflate_fixed__tmp
             (Some (EVar IDinflate_fixed_i))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,51%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDinflate_fixed_i
             (Some (EAdd (EVar IDinflate_fixed_i) (ENum (1))))),55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDinflate_fixed_z (Some (EAdd (ENum (1))
             (EVar IDinflate_fixed_z)))),58%positive)::
             (58%positive,AWeaken,17%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,(AAssign IDinflate_fixed_i
             (Some (EAdd (EVar IDinflate_fixed_i) (ENum (1))))),62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDinflate_fixed_z (Some (EAdd (ENum (1))
             (EVar IDinflate_fixed_z)))),65%positive)::
             (65%positive,AWeaken,13%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,(AAssign IDinflate_fixed_i
             (Some (EAdd (EVar IDinflate_fixed_i) (ENum (1))))),69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,(AAssign IDinflate_fixed_z (Some (EAdd (ENum (1))
             (EVar IDinflate_fixed_z)))),72%positive)::
             (72%positive,AWeaken,9%positive)::
             (73%positive,AWeaken,74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,(AAssign IDinflate_fixed_i
             (Some (EAdd (EVar IDinflate_fixed_i) (ENum (1))))),76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,(AAssign IDinflate_fixed_z (Some (EAdd (ENum (1))
             (EVar IDinflate_fixed_z)))),79%positive)::
             (79%positive,AWeaken,5%positive)::nil
|}.

Definition inflate_fixed_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_i) <= 0)%Z
    | 4%positive => (-1 * (s IDinflate_fixed_i) <= 0 /\ 1 * (s IDinflate_fixed_i) <= 0 /\ 1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 5%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) <= 0 /\ 1 * (s IDinflate_fixed_i) + -144 <= 0)%Z
    | 6%positive => (1 * (s IDinflate_fixed_i) + -144 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 144 <= 0)%Z
    | 7%positive => (-1 * (s IDinflate_fixed_i) + 144 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -144 <= 0)%Z
    | 8%positive => (1 * (s IDinflate_fixed_i) + -144 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 144 <= 0)%Z
    | 9%positive => (-1 * (s IDinflate_fixed_i) + 144 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -256 <= 0)%Z
    | 10%positive => (1 * (s IDinflate_fixed_i) + -256 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 256 <= 0)%Z
    | 11%positive => (-1 * (s IDinflate_fixed_i) + 256 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -256 <= 0)%Z
    | 12%positive => (1 * (s IDinflate_fixed_i) + -256 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 256 <= 0)%Z
    | 13%positive => (-1 * (s IDinflate_fixed_i) + 256 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -280 <= 0)%Z
    | 14%positive => (1 * (s IDinflate_fixed_i) + -280 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 280 <= 0)%Z
    | 15%positive => (-1 * (s IDinflate_fixed_i) + 280 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -280 <= 0)%Z
    | 16%positive => (1 * (s IDinflate_fixed_i) + -280 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 280 <= 0)%Z
    | 17%positive => (-1 * (s IDinflate_fixed_i) + 280 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -288 <= 0)%Z
    | 18%positive => (1 * (s IDinflate_fixed_i) + -288 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 288 <= 0)%Z
    | 19%positive => (-1 * (s IDinflate_fixed_i) + 288 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -288 <= 0)%Z
    | 20%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 21%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 22%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 23%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_i) <= 0)%Z
    | 24%positive => (-1 * (s IDinflate_fixed_i) <= 0 /\ 1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 25%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) <= 0 /\ 1 * (s IDinflate_fixed_i) + -30 <= 0)%Z
    | 26%positive => (1 * (s IDinflate_fixed_i) + -30 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 30 <= 0)%Z
    | 27%positive => (-1 * (s IDinflate_fixed_i) + 30 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -30 <= 0)%Z
    | 28%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 29%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 30%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 31%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 32%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 33%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed__tmp) <= 0 /\ -1 * (s IDinflate_fixed__tmp) <= 0)%Z
    | 34%positive => (-1 * (s IDinflate_fixed__tmp) <= 0 /\ 1 * (s IDinflate_fixed__tmp) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 35%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 36%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed__tmp) + -1 <= 0 /\ -1 * (s IDinflate_fixed__tmp) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDinflate_fixed__tmp) + 1 <= 0 /\ 1 * (s IDinflate_fixed__tmp) + -1 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 38%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 39%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 40%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 41%positive => (-1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -29 <= 0)%Z
    | 42%positive => (1 * (s IDinflate_fixed_i) + -29 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) <= 0)%Z
    | 43%positive => (-1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -29 <= 0)%Z
    | 44%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ 1 * (s IDinflate_fixed_i) + -30 <= 0)%Z
    | 45%positive => (1 * (s IDinflate_fixed_i) + -30 <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 46%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ 1 * (s IDinflate_fixed_i) + -30 <= 0)%Z
    | 47%positive => (1 * (s IDinflate_fixed_i) + -30 <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ -1 * (s IDinflate_fixed_z) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 49%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 50%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 51%positive => (-1 * (s IDinflate_fixed_z) <= 0)%Z
    | 52%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 280 <= 0 /\ 1 * (s IDinflate_fixed_i) + -287 <= 0)%Z
    | 53%positive => (1 * (s IDinflate_fixed_i) + -287 <= 0 /\ -1 * (s IDinflate_fixed_i) + 280 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 54%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 280 <= 0 /\ 1 * (s IDinflate_fixed_i) + -287 <= 0)%Z
    | 55%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 281 <= 0 /\ 1 * (s IDinflate_fixed_i) + -288 <= 0)%Z
    | 56%positive => (1 * (s IDinflate_fixed_i) + -288 <= 0 /\ -1 * (s IDinflate_fixed_i) + 281 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 57%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 281 <= 0 /\ 1 * (s IDinflate_fixed_i) + -288 <= 0)%Z
    | 58%positive => (1 * (s IDinflate_fixed_i) + -288 <= 0 /\ -1 * (s IDinflate_fixed_i) + 281 <= 0 /\ -1 * (s IDinflate_fixed_z) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 256 <= 0 /\ 1 * (s IDinflate_fixed_i) + -279 <= 0)%Z
    | 60%positive => (1 * (s IDinflate_fixed_i) + -279 <= 0 /\ -1 * (s IDinflate_fixed_i) + 256 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 61%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 256 <= 0 /\ 1 * (s IDinflate_fixed_i) + -279 <= 0)%Z
    | 62%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 257 <= 0 /\ 1 * (s IDinflate_fixed_i) + -280 <= 0)%Z
    | 63%positive => (1 * (s IDinflate_fixed_i) + -280 <= 0 /\ -1 * (s IDinflate_fixed_i) + 257 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 64%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 257 <= 0 /\ 1 * (s IDinflate_fixed_i) + -280 <= 0)%Z
    | 65%positive => (1 * (s IDinflate_fixed_i) + -280 <= 0 /\ -1 * (s IDinflate_fixed_i) + 257 <= 0 /\ -1 * (s IDinflate_fixed_z) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 144 <= 0 /\ 1 * (s IDinflate_fixed_i) + -255 <= 0)%Z
    | 67%positive => (1 * (s IDinflate_fixed_i) + -255 <= 0 /\ -1 * (s IDinflate_fixed_i) + 144 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 68%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 144 <= 0 /\ 1 * (s IDinflate_fixed_i) + -255 <= 0)%Z
    | 69%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 145 <= 0 /\ 1 * (s IDinflate_fixed_i) + -256 <= 0)%Z
    | 70%positive => (1 * (s IDinflate_fixed_i) + -256 <= 0 /\ -1 * (s IDinflate_fixed_i) + 145 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 71%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 145 <= 0 /\ 1 * (s IDinflate_fixed_i) + -256 <= 0)%Z
    | 72%positive => (1 * (s IDinflate_fixed_i) + -256 <= 0 /\ -1 * (s IDinflate_fixed_i) + 145 <= 0 /\ -1 * (s IDinflate_fixed_z) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -143 <= 0)%Z
    | 74%positive => (1 * (s IDinflate_fixed_i) + -143 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) <= 0)%Z
    | 75%positive => (-1 * (s IDinflate_fixed_i) <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0 /\ 1 * (s IDinflate_fixed_i) + -143 <= 0)%Z
    | 76%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ 1 * (s IDinflate_fixed_i) + -144 <= 0)%Z
    | 77%positive => (1 * (s IDinflate_fixed_i) + -144 <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ -1 * (s IDinflate_fixed_z) <= 0)%Z
    | 78%positive => (-1 * (s IDinflate_fixed_z) <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ 1 * (s IDinflate_fixed_i) + -144 <= 0)%Z
    | 79%positive => (1 * (s IDinflate_fixed_i) + -144 <= 0 /\ -1 * (s IDinflate_fixed_i) + 1 <= 0 /\ -1 * (s IDinflate_fixed_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition inflate_fixed_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((318 # 1))%Q
    | 2%positive => ((318 # 1) + (s IDinflate_fixed_z))%Q
    | 3%positive => ((30 # 1) + (s IDinflate_fixed_z)
                     + max0(288 - (s IDinflate_fixed_i)))%Q
    | 4%positive => ((30 # 1) + (s IDinflate_fixed_z)
                     + max0(288 - (s IDinflate_fixed_i)))%Q
    | 5%positive => ((30 # 1) + (s IDinflate_fixed_z)
                     + max0(288 - (s IDinflate_fixed_i)))%Q
    | 6%positive => ((30 # 1) + (s IDinflate_fixed_z)
                     + max0(288 - (s IDinflate_fixed_i)))%Q
    | 7%positive => ((30 # 1) + (s IDinflate_fixed_z)
                     + max0(288 - (s IDinflate_fixed_i)))%Q
    | 8%positive => ((30 # 1) + (s IDinflate_fixed_z)
                     + max0(288 - (s IDinflate_fixed_i)))%Q
    | 9%positive => ((61 # 2) + (s IDinflate_fixed_z)
                     + (1 # 2) * max0(287 - (s IDinflate_fixed_i))
                     + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 10%positive => ((61 # 2) + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i))
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 11%positive => ((61 # 2) + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i))
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 12%positive => ((61 # 2) + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i))
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 13%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 14%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 15%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 16%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 17%positive => ((318 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 18%positive => ((318 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 19%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 20%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 21%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 22%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 23%positive => ((s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 24%positive => ((s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 25%positive => ((s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 26%positive => ((s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 27%positive => ((s IDinflate_fixed_z))%Q
    | 28%positive => ((s IDinflate_fixed_z))%Q
    | 29%positive => ((s IDinflate_fixed_z))%Q
    | 30%positive => ((s IDinflate_fixed_z))%Q
    | 31%positive => ((s IDinflate_fixed_z))%Q
    | 32%positive => ((s IDinflate_fixed_z))%Q
    | 33%positive => ((s IDinflate_fixed_z))%Q
    | 34%positive => ((s IDinflate_fixed_z))%Q
    | 35%positive => ((s IDinflate_fixed_z))%Q
    | 36%positive => ((s IDinflate_fixed_z))%Q
    | 37%positive => ((s IDinflate_fixed_z))%Q
    | 38%positive => ((s IDinflate_fixed_z))%Q
    | 39%positive => ((s IDinflate_fixed_z))%Q
    | 40%positive => ((s IDinflate_fixed_z))%Q
    | 41%positive => ((s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 42%positive => ((1 # 1) + (s IDinflate_fixed_z)
                      + max0(29 - (s IDinflate_fixed_i)))%Q
    | 43%positive => ((1 # 1) + (s IDinflate_fixed_z)
                      + max0(29 - (s IDinflate_fixed_i)))%Q
    | 44%positive => ((1 # 1) + (s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 45%positive => ((1 # 1) + (s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 46%positive => ((1 # 1) + (s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 47%positive => ((s IDinflate_fixed_z)
                      + max0(30 - (s IDinflate_fixed_i)))%Q
    | 48%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 49%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 50%positive => ((30 # 1) + (s IDinflate_fixed_z))%Q
    | 51%positive => ((s IDinflate_fixed_z))%Q
    | 52%positive => ((318 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 53%positive => ((318 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 54%positive => ((318 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 55%positive => ((319 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 56%positive => ((319 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 57%positive => ((319 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 58%positive => ((318 # 1) - (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z))%Q
    | 59%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 60%positive => ((349 # 2) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i)))%Q
    | 61%positive => ((349 # 2) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i)))%Q
    | 62%positive => ((175 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 63%positive => ((175 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 64%positive => ((175 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 65%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 66%positive => ((61 # 2) + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i))
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 67%positive => ((349 # 2) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i)))%Q
    | 68%positive => ((349 # 2) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(287 - (s IDinflate_fixed_i)))%Q
    | 69%positive => ((175 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 70%positive => ((175 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 71%positive => ((175 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 72%positive => ((174 # 1) - (1 # 2) * (s IDinflate_fixed_i)
                      + (s IDinflate_fixed_z)
                      + (1 # 2) * max0(288 - (s IDinflate_fixed_i)))%Q
    | 73%positive => ((30 # 1) + (s IDinflate_fixed_z)
                      + max0(288 - (s IDinflate_fixed_i)))%Q
    | 74%positive => ((31 # 1) + (s IDinflate_fixed_z)
                      + max0(287 - (s IDinflate_fixed_i)))%Q
    | 75%positive => ((31 # 1) + (s IDinflate_fixed_z)
                      + max0(287 - (s IDinflate_fixed_i)))%Q
    | 76%positive => ((31 # 1) + (s IDinflate_fixed_z)
                      + max0(288 - (s IDinflate_fixed_i)))%Q
    | 77%positive => ((31 # 1) + (s IDinflate_fixed_z)
                      + max0(288 - (s IDinflate_fixed_i)))%Q
    | 78%positive => ((31 # 1) + (s IDinflate_fixed_z)
                      + max0(288 - (s IDinflate_fixed_i)))%Q
    | 79%positive => ((30 # 1) + (s IDinflate_fixed_z)
                      + max0(288 - (s IDinflate_fixed_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition inflate_fixed_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-0.5 0*) F_max0_pre_decrement (288
                                                      - (s IDinflate_fixed_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (287
                                                                    - 
                                                                    (s IDinflate_fixed_i))) (F_check_ge (287
                                                                    - (s IDinflate_fixed_i)) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (288
                                                                    - 
                                                                    (s IDinflate_fixed_i))) (F_check_ge (288
                                                                    - (s IDinflate_fixed_i)) (0))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (288
                                                             - (s IDinflate_fixed_i)) (287
                                                                    - (s IDinflate_fixed_i)));
                      (*-1 0*) F_max0_ge_0 (287 - (s IDinflate_fixed_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (288
                                                                    - (s IDinflate_fixed_i)) (0))) (F_max0_ge_0 (288
                                                                    - (s IDinflate_fixed_i)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (30
                                                             - (s IDinflate_fixed_i)) (29
                                                                    - (s IDinflate_fixed_i)));
                      (*-1 0*) F_max0_ge_0 (29 - (s IDinflate_fixed_i))]
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
    | 41%positive => [(*-1 0*) F_max0_pre_decrement (30
                                                     - (s IDinflate_fixed_i)) (1)]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-30 0*) F_one]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*0 0.5*) F_max0_pre_decrement (288
                                                      - (s IDinflate_fixed_i)) (1)]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (288
                                                                    - 
                                                                    (s IDinflate_fixed_i))) (F_check_ge (288
                                                                    - (s IDinflate_fixed_i)) (0))]
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (287
                                                                    - (s IDinflate_fixed_i)) (0))) (F_max0_ge_0 (287
                                                                    - (s IDinflate_fixed_i)))]
    | 73%positive => [(*-1 0*) F_max0_pre_decrement (288
                                                     - (s IDinflate_fixed_i)) (1)]
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | _ => []
  end.


Theorem inflate_fixed_ai_correct:
  forall s p' s', steps (g_start inflate_fixed) s (g_edges inflate_fixed) p' s' -> inflate_fixed_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem inflate_fixed_pot_correct:
  forall s p' s',
    steps (g_start inflate_fixed) s (g_edges inflate_fixed) p' s' ->
    (inflate_fixed_pot (g_start inflate_fixed) s >= inflate_fixed_pot p' s')%Q.
Proof.
  check_lp inflate_fixed_ai_correct inflate_fixed_hints.
Qed.

