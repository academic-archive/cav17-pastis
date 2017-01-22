Require Import pasta.Pasta.

Notation IDfft_short_z := 1%positive.
Notation IDfft_short__tmp := 2%positive.
Notation IDfft_short_b := 3%positive.
Notation IDfft_short_i := 4%positive.
Notation IDfft_short_j := 5%positive.
Notation IDfft_short_k := 6%positive.
Notation IDfft_short_buffer := 7%positive.
Notation IDfft_short_chn := 8%positive.
Notation IDfft_short_x_real := 9%positive.
Definition fft_short : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDfft_short_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfft_short__tmp
             (Some (EVar IDfft_short_chn))),3%positive)::
             (3%positive,(AAssign IDfft_short_b (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDfft_short_b) s) <
             (eval (ENum (3)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDfft_short_b) s) >=
             (eval (ENum (3)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDfft_short_k (Some (EMul (ENum (192))
             (EAdd (EVar IDfft_short_b) (ENum (1)))))),11%positive)::
             (11%positive,(AAssign IDfft_short_j (Some (ENum (31)))),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDfft_short__tmp)
             s) < (eval (ENum (2)) s))%Z)),41%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDfft_short__tmp)
             s) >= (eval (ENum (2)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDfft_short__tmp)
             s) = (eval (ENum (2)) s))%Z)),28%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDfft_short__tmp)
             s) <> (eval (ENum (2)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDfft_short_i None),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDfft_short_j
             (Some (EAdd (EVar IDfft_short_j) (ENum (-1))))),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_short_j)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),25%positive)::
             (22%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_short_j)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,37%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDfft_short_z (Some (EAdd (ENum (1))
             (EVar IDfft_short_z)))),18%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDfft_short_i None),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDfft_short_j
             (Some (EAdd (EVar IDfft_short_j) (ENum (-1))))),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_short_j)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),38%positive)::
             (34%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_short_j)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,50%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDfft_short_z (Some (EAdd (ENum (1))
             (EVar IDfft_short_z)))),30%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDfft_short_i None),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDfft_short_j
             (Some (EAdd (EVar IDfft_short_j) (ENum (-1))))),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_short_j)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),56%positive)::
             (47%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_short_j)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDfft_short_b
             (Some (EAdd (EVar IDfft_short_b) (ENum (1))))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDfft_short_z (Some (EAdd (ENum (1))
             (EVar IDfft_short_z)))),55%positive)::
             (55%positive,AWeaken,6%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDfft_short_z (Some (EAdd (ENum (1))
             (EVar IDfft_short_z)))),43%positive)::nil
|}.

Definition fft_short_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_z) <= 0)%Z
    | 4%positive => (1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_b) <= 0)%Z
    | 5%positive => (-1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_z) <= 0)%Z
    | 6%positive => (-1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0)%Z
    | 7%positive => (-1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) + 3 <= 0)%Z
    | 8%positive => (-1 * (s IDfft_short_b) + 3 <= 0 /\ -1 * (s IDfft_short_z) <= 0)%Z
    | 9%positive => (-1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0)%Z
    | 10%positive => (1 * (s IDfft_short_b) + -2 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0)%Z
    | 11%positive => (-1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 12%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0)%Z
    | 13%positive => (-1 * (s IDfft_short_j) + 31 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 14%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0)%Z
    | 15%positive => (-1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 16%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ -1 * (s IDfft_short__tmp) + 3 <= 0)%Z
    | 17%positive => (-1 * (s IDfft_short__tmp) + 3 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 18%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 3 <= 0)%Z
    | 19%positive => (-1 * (s IDfft_short__tmp) + 3 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_z) <= 0)%Z
    | 20%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 3 <= 0)%Z
    | 21%positive => (-1 * (s IDfft_short__tmp) + 3 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) <= 0)%Z
    | 22%positive => (-1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 3 <= 0)%Z
    | 23%positive => (-1 * (s IDfft_short__tmp) + 3 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0)%Z
    | 24%positive => (1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 3 <= 0)%Z
    | 25%positive => (-1 * (s IDfft_short__tmp) + 3 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDfft_short_j) + 1 <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 3 <= 0)%Z
    | 27%positive => (-1 * (s IDfft_short__tmp) + 3 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ 1 * (s IDfft_short__tmp) + -2 <= 0)%Z
    | 29%positive => (1 * (s IDfft_short__tmp) + -2 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 30%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ 1 * (s IDfft_short__tmp) + -2 <= 0)%Z
    | 31%positive => (1 * (s IDfft_short__tmp) + -2 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_z) <= 0)%Z
    | 32%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ 1 * (s IDfft_short__tmp) + -2 <= 0)%Z
    | 33%positive => (1 * (s IDfft_short__tmp) + -2 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) <= 0)%Z
    | 34%positive => (-1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ 1 * (s IDfft_short__tmp) + -2 <= 0)%Z
    | 35%positive => (1 * (s IDfft_short__tmp) + -2 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0)%Z
    | 36%positive => (1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ 1 * (s IDfft_short__tmp) + -2 <= 0)%Z
    | 37%positive => (-1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0)%Z
    | 38%positive => (1 * (s IDfft_short__tmp) + -2 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDfft_short_j) + 1 <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ 1 * (s IDfft_short__tmp) + -2 <= 0)%Z
    | 40%positive => (1 * (s IDfft_short__tmp) + -2 <= 0 /\ -1 * (s IDfft_short__tmp) + 2 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ 1 * (s IDfft_short__tmp) + -1 <= 0)%Z
    | 42%positive => (1 * (s IDfft_short__tmp) + -1 <= 0 /\ -1 * (s IDfft_short_j) + 31 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_b) + -2 <= 0 /\ 1 * (s IDfft_short_k) + -576 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 43%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short__tmp) + -1 <= 0)%Z
    | 44%positive => (1 * (s IDfft_short__tmp) + -1 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_z) <= 0)%Z
    | 45%positive => (-1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -31 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short__tmp) + -1 <= 0)%Z
    | 46%positive => (1 * (s IDfft_short__tmp) + -1 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) <= 0)%Z
    | 47%positive => (-1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short__tmp) + -1 <= 0)%Z
    | 48%positive => (1 * (s IDfft_short__tmp) + -1 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0)%Z
    | 49%positive => (1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short__tmp) + -1 <= 0)%Z
    | 50%positive => (-1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0)%Z
    | 51%positive => (1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0)%Z
    | 52%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_b) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDfft_short_b) + 1 <= 0 /\ 1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0)%Z
    | 54%positive => (-1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ 1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_b) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDfft_short_b) + 1 <= 0 /\ 1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_j) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) + 1 <= 0)%Z
    | 56%positive => (1 * (s IDfft_short__tmp) + -1 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDfft_short_j) + 1 <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ 1 * (s IDfft_short__tmp) + -1 <= 0)%Z
    | 58%positive => (1 * (s IDfft_short__tmp) + -1 <= 0 /\ -1 * (s IDfft_short_b) <= 0 /\ -1 * (s IDfft_short_k) + 192 <= 0 /\ -1 * (s IDfft_short_z) <= 0 /\ 1 * (s IDfft_short_j) + -30 <= 0 /\ -1 * (s IDfft_short_j) + 1 <= 0)%Z
    | _ => False
  end.

Definition fft_short_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((93 # 1))%Q
    | 2%positive => ((93 # 1) + (s IDfft_short_z))%Q
    | 3%positive => ((93 # 1) + (s IDfft_short_z))%Q
    | 4%positive => ((s IDfft_short_z)
                     + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 5%positive => ((s IDfft_short_z)
                     + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 6%positive => ((s IDfft_short_z)
                     + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 7%positive => ((s IDfft_short_z)
                     + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 8%positive => ((s IDfft_short_z))%Q
    | 9%positive => ((s IDfft_short_z)
                     + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 10%positive => ((s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 11%positive => ((s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 12%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 13%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 14%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 15%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 16%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 17%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 18%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 19%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 20%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 21%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 22%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 23%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 24%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 25%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 26%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 27%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 28%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 29%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 30%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 31%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 32%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 33%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 34%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 35%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 36%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 37%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 38%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 39%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 40%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 41%positive => (-(31 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b)))%Q
    | 42%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 43%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 44%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 45%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 46%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 47%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 48%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 49%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 50%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 51%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 52%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 53%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 54%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 55%positive => ((s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(3 - (s IDfft_short_b))
                      - max0((s IDfft_short_j)))%Q
    | 56%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 57%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | 58%positive => ((1 # 1) + (s IDfft_short_j) + (s IDfft_short_z)
                      + (31 # 1) * max0(2 - (s IDfft_short_b)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fft_short_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-31 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDfft_short_b)) (2
                                                                    - (s IDfft_short_b)));
                     (*-31 0*) F_max0_ge_0 (2 - (s IDfft_short_b))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-31 0*) F_max0_pre_decrement (3 - (s IDfft_short_b)) (1)]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfft_short_j)) (-1
                                                                    + (s IDfft_short_j)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDfft_short_j))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-31 0*) F_max0_pre_decrement (3 - (s IDfft_short_b)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfft_short_j)) (-1
                                                                    + (s IDfft_short_j)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDfft_short_j))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-31 0*) F_max0_pre_decrement (3 - (s IDfft_short_b)) (1)]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfft_short_j)) (-1
                                                                    + (s IDfft_short_j)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDfft_short_j))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDfft_short_j)) (0))) (F_max0_ge_0 ((s IDfft_short_j)))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | _ => []
  end.


Theorem fft_short_ai_correct:
  forall s p' s', steps (g_start fft_short) s (g_edges fft_short) p' s' -> fft_short_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fft_short_pot_correct:
  forall s p' s',
    steps (g_start fft_short) s (g_edges fft_short) p' s' ->
    (fft_short_pot (g_start fft_short) s >= fft_short_pot p' s')%Q.
Proof.
  check_lp fft_short_ai_correct fft_short_hints.
Qed.

