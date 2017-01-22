Require Import pasta.Pasta.

Notation IDinflate_block_z := 1%positive.
Notation IDinflate_block__tmp := 2%positive.
Notation IDinflate_block_b := 3%positive.
Notation IDinflate_block_bb := 4%positive.
Notation IDinflate_block_bk := 5%positive.
Notation IDinflate_block_e_dref := 6%positive.
Notation IDinflate_block_incnt := 7%positive.
Notation IDinflate_block_k := 8%positive.
Notation IDinflate_block_t := 9%positive.
Notation IDinflate_block_e := 10%positive.
Definition inflate_block : graph := {|
  g_start := 1%positive;
  g_end := 76%positive;
  g_edges := (1%positive,(AAssign IDinflate_block_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_k)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDinflate_block_b
             (Some (EVar IDinflate_block_bb))),5%positive)::
             (5%positive,(AAssign IDinflate_block_k
             (Some (EVar IDinflate_block_bk))),6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_k)
             s) < (eval (ENum (1)) s))%Z)),60%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_k)
             s) >= (eval (ENum (1)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDinflate_block_e_dref None),11%positive)::
             (11%positive,(AAssign IDinflate_block_b None),12%positive)::
             (12%positive,(AAssign IDinflate_block_k
             (Some (ESub (EVar IDinflate_block_k) (ENum (1))))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_k)
             s) < (eval (ENum (2)) s))%Z)),44%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_k)
             s) >= (eval (ENum (2)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDinflate_block_t None),18%positive)::
             (18%positive,(AAssign IDinflate_block_b None),19%positive)::
             (19%positive,(AAssign IDinflate_block_k
             (Some (ESub (EVar IDinflate_block_k) (ENum (2))))),20%positive)::
             (20%positive,(AAssign IDinflate_block_bb
             (Some (EVar IDinflate_block_b))),21%positive)::
             (21%positive,(AAssign IDinflate_block_bk
             (Some (EVar IDinflate_block_k))),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_t)
             s) = (eval (ENum (2)) s))%Z)),40%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_t)
             s) <> (eval (ENum (2)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_t)
             s) = (eval (ENum (0)) s))%Z)),36%positive)::
             (25%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_t)
             s) <> (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_t)
             s) = (eval (ENum (1)) s))%Z)),32%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDinflate_block_t)
             s) <> (eval (ENum (1)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDinflate_block__tmp (Some (ENum (2)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,76%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDinflate_block__tmp None),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,76%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDinflate_block__tmp None),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,76%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDinflate_block__tmp None),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,76%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDinflate_block_incnt
             (Some (EAdd (EVar IDinflate_block_incnt) (ENum (-1))))),
             46%positive)::(46%positive,AWeaken,47%positive)::
             (47%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDinflate_block_incnt) (ENum (-1)))
             s) < (eval (ENum (0)) s))%Z)),49%positive)::
             (47%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDinflate_block_incnt) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),48%positive)::
             (48%positive,AWeaken,51%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,57%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDinflate_block_b None),52%positive)::
             (52%positive,(AAssign IDinflate_block_k
             (Some (EAdd (EVar IDinflate_block_k) (ENum (8))))),53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDinflate_block_z (Some (EAdd (ENum (1))
             (EVar IDinflate_block_z)))),56%positive)::
             (56%positive,AWeaken,15%positive)::
             (57%positive,(AAssign IDinflate_block__tmp (Some (ENum (1)))),
             58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,AWeaken,76%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AAssign IDinflate_block_incnt
             (Some (EAdd (EVar IDinflate_block_incnt) (ENum (-1))))),
             62%positive)::(62%positive,AWeaken,63%positive)::
             (63%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDinflate_block_incnt) (ENum (-1)))
             s) < (eval (ENum (0)) s))%Z)),65%positive)::
             (63%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDinflate_block_incnt) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),64%positive)::
             (64%positive,AWeaken,67%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,ANone,73%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDinflate_block_b None),68%positive)::
             (68%positive,(AAssign IDinflate_block_k
             (Some (EAdd (EVar IDinflate_block_k) (ENum (8))))),69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,(AAssign IDinflate_block_z (Some (EAdd (ENum (1))
             (EVar IDinflate_block_z)))),72%positive)::
             (72%positive,AWeaken,8%positive)::
             (73%positive,(AAssign IDinflate_block__tmp (Some (ENum (1)))),
             74%positive)::(74%positive,ANone,75%positive)::
             (75%positive,AWeaken,76%positive)::nil
|}.

Definition inflate_block_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 4%positive => (-1 * (s IDinflate_block_k) <= 0 /\ 1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 5%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 6%positive => (1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 7%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_z) <= 0)%Z
    | 8%positive => (-1 * (s IDinflate_block_z) <= 0)%Z
    | 9%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDinflate_block_k) + 1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 11%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDinflate_block_k) + 1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 13%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 14%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 15%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 16%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) + 2 <= 0)%Z
    | 17%positive => (-1 * (s IDinflate_block_k) + 2 <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 18%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) + 2 <= 0)%Z
    | 19%positive => (-1 * (s IDinflate_block_k) + 2 <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 20%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 21%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 22%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0)%Z
    | 23%positive => (-1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 24%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0)%Z
    | 25%positive => (-1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 26%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0)%Z
    | 27%positive => (-1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 28%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0)%Z
    | 29%positive => (-1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 30%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block__tmp) + -2 <= 0 /\ -1 * (s IDinflate_block__tmp) + 2 <= 0)%Z
    | 31%positive => (-1 * (s IDinflate_block__tmp) + 2 <= 0 /\ 1 * (s IDinflate_block__tmp) + -2 <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 32%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block_t) + -1 <= 0 /\ -1 * (s IDinflate_block_t) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDinflate_block_t) + 1 <= 0 /\ 1 * (s IDinflate_block_t) + -1 <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 34%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block_t) + -1 <= 0 /\ -1 * (s IDinflate_block_t) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDinflate_block_t) + 1 <= 0 /\ 1 * (s IDinflate_block_t) + -1 <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 36%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block_t) <= 0 /\ -1 * (s IDinflate_block_t) <= 0)%Z
    | 37%positive => (-1 * (s IDinflate_block_t) <= 0 /\ 1 * (s IDinflate_block_t) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 38%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block_t) <= 0 /\ -1 * (s IDinflate_block_t) <= 0)%Z
    | 39%positive => (-1 * (s IDinflate_block_t) <= 0 /\ 1 * (s IDinflate_block_t) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 40%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block_t) + -2 <= 0 /\ -1 * (s IDinflate_block_t) + 2 <= 0)%Z
    | 41%positive => (-1 * (s IDinflate_block_t) + 2 <= 0 /\ 1 * (s IDinflate_block_t) + -2 <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 42%positive => (-1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ 1 * (s IDinflate_block_t) + -2 <= 0 /\ -1 * (s IDinflate_block_t) + 2 <= 0)%Z
    | 43%positive => (-1 * (s IDinflate_block_t) + 2 <= 0 /\ 1 * (s IDinflate_block_t) + -2 <= 0 /\ -1 * (s IDinflate_block_bk) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 44%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0)%Z
    | 45%positive => (1 * (s IDinflate_block_k) + -1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 46%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0)%Z
    | 47%positive => (1 * (s IDinflate_block_k) + -1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 48%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0 /\ -1 * (s IDinflate_block_incnt) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0 /\ 1 * (s IDinflate_block_incnt) <= 0)%Z
    | 50%positive => (1 * (s IDinflate_block_incnt) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 51%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0)%Z
    | 52%positive => (1 * (s IDinflate_block_k) + -1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0)%Z
    | 53%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -9 <= 0 /\ -1 * (s IDinflate_block_k) + 8 <= 0)%Z
    | 54%positive => (-1 * (s IDinflate_block_k) + 8 <= 0 /\ 1 * (s IDinflate_block_k) + -9 <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 55%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -9 <= 0 /\ -1 * (s IDinflate_block_k) + 8 <= 0)%Z
    | 56%positive => (-1 * (s IDinflate_block_k) + 8 <= 0 /\ 1 * (s IDinflate_block_k) + -9 <= 0 /\ -1 * (s IDinflate_block_z) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0 /\ 1 * (s IDinflate_block_incnt) <= 0)%Z
    | 58%positive => (1 * (s IDinflate_block_incnt) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ 1 * (s IDinflate_block__tmp) + -1 <= 0 /\ -1 * (s IDinflate_block__tmp) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDinflate_block__tmp) + 1 <= 0 /\ 1 * (s IDinflate_block__tmp) + -1 <= 0 /\ -1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -1 <= 0 /\ 1 * (s IDinflate_block_incnt) <= 0)%Z
    | 60%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0)%Z
    | 61%positive => (1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 62%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0)%Z
    | 63%positive => (1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 64%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_incnt) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0 /\ 1 * (s IDinflate_block_incnt) <= 0)%Z
    | 66%positive => (1 * (s IDinflate_block_incnt) <= 0 /\ 1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 67%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0)%Z
    | 68%positive => (1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 69%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -8 <= 0)%Z
    | 70%positive => (1 * (s IDinflate_block_k) + -8 <= 0 /\ -1 * (s IDinflate_block_z) <= 0)%Z
    | 71%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) + -8 <= 0)%Z
    | 72%positive => (1 * (s IDinflate_block_k) + -8 <= 0 /\ -1 * (s IDinflate_block_z) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0 /\ 1 * (s IDinflate_block_incnt) <= 0)%Z
    | 74%positive => (1 * (s IDinflate_block_incnt) <= 0 /\ 1 * (s IDinflate_block_k) <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block__tmp) + -1 <= 0 /\ -1 * (s IDinflate_block__tmp) + 1 <= 0)%Z
    | 75%positive => (-1 * (s IDinflate_block__tmp) + 1 <= 0 /\ 1 * (s IDinflate_block__tmp) + -1 <= 0 /\ -1 * (s IDinflate_block_z) <= 0 /\ 1 * (s IDinflate_block_k) <= 0 /\ 1 * (s IDinflate_block_incnt) <= 0)%Z
    | 76%positive => (-1 * (s IDinflate_block_z) <= 0)%Z
    | _ => False
  end.

Definition inflate_block_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0(10 - (s IDinflate_block_bk)))%Q
    | 2%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_bk)))%Q
    | 3%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_bk)))%Q
    | 4%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_bk)))%Q
    | 5%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_bk)))%Q
    | 6%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 7%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 8%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 9%positive => ((s IDinflate_block_z)
                     + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 10%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 11%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 12%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 13%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 14%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 15%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 16%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 17%positive => ((s IDinflate_block_z))%Q
    | 18%positive => ((s IDinflate_block_z))%Q
    | 19%positive => ((s IDinflate_block_z))%Q
    | 20%positive => ((s IDinflate_block_z))%Q
    | 21%positive => ((s IDinflate_block_z))%Q
    | 22%positive => ((s IDinflate_block_z))%Q
    | 23%positive => ((s IDinflate_block_z))%Q
    | 24%positive => ((s IDinflate_block_z))%Q
    | 25%positive => ((s IDinflate_block_z))%Q
    | 26%positive => ((s IDinflate_block_z))%Q
    | 27%positive => ((s IDinflate_block_z))%Q
    | 28%positive => ((s IDinflate_block_z))%Q
    | 29%positive => ((s IDinflate_block_z))%Q
    | 30%positive => ((s IDinflate_block_z))%Q
    | 31%positive => ((s IDinflate_block_z))%Q
    | 32%positive => ((s IDinflate_block_z))%Q
    | 33%positive => ((s IDinflate_block_z))%Q
    | 34%positive => ((s IDinflate_block_z))%Q
    | 35%positive => ((s IDinflate_block_z))%Q
    | 36%positive => ((s IDinflate_block_z))%Q
    | 37%positive => ((s IDinflate_block_z))%Q
    | 38%positive => ((s IDinflate_block_z))%Q
    | 39%positive => ((s IDinflate_block_z))%Q
    | 40%positive => ((s IDinflate_block_z))%Q
    | 41%positive => ((s IDinflate_block_z))%Q
    | 42%positive => ((s IDinflate_block_z))%Q
    | 43%positive => ((s IDinflate_block_z))%Q
    | 44%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 45%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 46%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(9 - (s IDinflate_block_k)))%Q
    | 47%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 48%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 49%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 50%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 51%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 52%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 53%positive => ((17 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 54%positive => ((17 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 55%positive => ((17 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 56%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 57%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 58%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 59%positive => ((9 # 8) - (1 # 8) * (s IDinflate_block_k)
                      + (s IDinflate_block_z))%Q
    | 60%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 61%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 62%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 63%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 64%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 65%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 66%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(2 - (s IDinflate_block_k)))%Q
    | 67%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(2 - (s IDinflate_block_k)))%Q
    | 68%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(2 - (s IDinflate_block_k)))%Q
    | 69%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 70%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 71%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 72%positive => ((s IDinflate_block_z)
                      + (1 # 8) * max0(10 - (s IDinflate_block_k)))%Q
    | 73%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(2 - (s IDinflate_block_k)))%Q
    | 74%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(2 - (s IDinflate_block_k)))%Q
    | 75%positive => ((1 # 1) + (s IDinflate_block_z)
                      + (1 # 8) * max0(2 - (s IDinflate_block_k)))%Q
    | 76%positive => ((s IDinflate_block_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition inflate_block_hints (p : node) (s : state) := 
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
    | 16%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge (9
                                                                 - (s IDinflate_block_k)) (1
                                                                    - (s IDinflate_block_k)));
                      (*-0.125 0*) F_max0_ge_0 (1 - (s IDinflate_block_k))]
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
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => [(*0 0.125*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                                    - (s IDinflate_block_k))) (F_check_ge (9
                                                                    - (s IDinflate_block_k)) (0))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDinflate_block_z))) (F_check_ge ((s IDinflate_block_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDinflate_block_z)) (0))) (F_max0_ge_0 ((s IDinflate_block_z)));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDinflate_block_k)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDinflate_block_k)))]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*-1 0*) F_one;
                      (*-0.125 0*) F_max0_pre_decrement (9
                                                         - (s IDinflate_block_k)) (8);
                      (*-0.125 0*) F_max0_ge_0 (1 - (s IDinflate_block_k));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - (s IDinflate_block_k)) (0))) (F_max0_ge_0 (9
                                                                    - (s IDinflate_block_k)))]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => [(*-0.125 0*) F_max0_pre_decrement (10
                                                         - (s IDinflate_block_k)) (8)]
    | 65%positive => [(*0 0.125*) F_max0_pre_decrement (10
                                                        - (s IDinflate_block_k)) (8)]
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*-1 0*) F_one;
                      (*-0.125 0*) F_max0_ge_0 (2 - (s IDinflate_block_k))]
    | 76%positive => []
    | _ => []
  end.


Theorem inflate_block_ai_correct:
  forall s p' s', steps (g_start inflate_block) s (g_edges inflate_block) p' s' -> inflate_block_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem inflate_block_pot_correct:
  forall s p' s',
    steps (g_start inflate_block) s (g_edges inflate_block) p' s' ->
    (inflate_block_pot (g_start inflate_block) s >= inflate_block_pot p' s')%Q.
Proof.
  check_lp inflate_block_ai_correct inflate_block_hints.
Qed.

