Require Import pasta.Pasta.

Notation IDbwbse_sort_z := 1%positive.
Notation IDbwbse_sort__tmp := 2%positive.
Notation IDbwbse_sort_ch := 3%positive.
Notation IDbwbse_sort_j := 4%positive.
Notation IDbwbse_sort_sum := 5%positive.
Notation IDbwbse_sort_N := 6%positive.
Notation IDbwbse_sort_buffer := 7%positive.
Notation IDbwbse_sort_indices := 8%positive.
Definition bwbse_sort : graph := {|
  g_start := 1%positive;
  g_end := 29%positive;
  g_edges := (1%positive,(AAssign IDbwbse_sort_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_j) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_ch)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort__tmp)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDbwbse_sort__tmp
             (Some (EVar IDbwbse_sort_N))),7%positive)::
             (7%positive,(AAssign IDbwbse_sort_sum (Some (ENum (0)))),
             8%positive)::
             (8%positive,(AAssign IDbwbse_sort_j (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_j) s) <
             (eval (EVar IDbwbse_sort__tmp) s))%Z)),53%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_j)
             s) >= (eval (EVar IDbwbse_sort__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDbwbse_sort_ch (Some (ENum (0)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_ch)
             s) <= (eval (ENum (255)) s))%Z)),45%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_ch)
             s) > (eval (ENum (255)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDbwbse_sort_j (Some (ENum (0)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_j) s) <
             (eval (EVar IDbwbse_sort__tmp) s))%Z)),38%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_j)
             s) >= (eval (EVar IDbwbse_sort__tmp) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AAssign IDbwbse_sort_sum (Some (ENum (0)))),
             24%positive)::
             (24%positive,(AAssign IDbwbse_sort_ch (Some (ENum (0)))),
             25%positive)::(25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_ch)
             s) <= (eval (ENum (255)) s))%Z)),30%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDbwbse_sort_ch)
             s) > (eval (ENum (255)) s))%Z)),28%positive)::
             (28%positive,AWeaken,29%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDbwbse_sort_sum None),33%positive)::
             (33%positive,(AAssign IDbwbse_sort_ch
             (Some (EAdd (EVar IDbwbse_sort_ch) (ENum (1))))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDbwbse_sort_z (Some (EAdd (ENum (1))
             (EVar IDbwbse_sort_z)))),37%positive)::
             (37%positive,AWeaken,27%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDbwbse_sort_j
             (Some (EAdd (EVar IDbwbse_sort_j) (ENum (1))))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDbwbse_sort_z (Some (EAdd (ENum (1))
             (EVar IDbwbse_sort_z)))),44%positive)::
             (44%positive,AWeaken,21%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AAssign IDbwbse_sort_sum None),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDbwbse_sort_ch
             (Some (EAdd (EVar IDbwbse_sort_ch) (ENum (1))))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDbwbse_sort_z (Some (EAdd (ENum (1))
             (EVar IDbwbse_sort_z)))),52%positive)::
             (52%positive,AWeaken,16%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDbwbse_sort_j
             (Some (EAdd (EVar IDbwbse_sort_j) (ENum (1))))),56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDbwbse_sort_z (Some (EAdd (ENum (1))
             (EVar IDbwbse_sort_z)))),59%positive)::
             (59%positive,AWeaken,11%positive)::nil
|}.

Definition bwbse_sort_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 4%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 5%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDbwbse_sort__tmp) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 7%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 8%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0)%Z
    | 9%positive => (-1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 10%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0)%Z
    | 11%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 12%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 13%positive => (1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 14%positive => (1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 15%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0)%Z
    | 16%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0)%Z
    | 17%positive => (1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0)%Z
    | 18%positive => (-1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0)%Z
    | 19%positive => (1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 20%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0)%Z
    | 21%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0)%Z
    | 22%positive => (-1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 23%positive => (1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0)%Z
    | 24%positive => (-1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0)%Z
    | 25%positive => (-1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 26%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0)%Z
    | 27%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0)%Z
    | 28%positive => (1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0)%Z
    | 29%positive => (-1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0)%Z
    | 30%positive => (1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -255 <= 0)%Z
    | 31%positive => (1 * (s IDbwbse_sort_ch) + -255 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 32%positive => (1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -255 <= 0)%Z
    | 33%positive => (1 * (s IDbwbse_sort_ch) + -255 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 34%positive => (1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDbwbse_sort_ch) + 1 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 36%positive => (1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDbwbse_sort_ch) + 1 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0)%Z
    | 40%positive => (-1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0)%Z
    | 42%positive => (-1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0)%Z
    | 43%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0)%Z
    | 44%positive => (-1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 256 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_z) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -255 <= 0)%Z
    | 46%positive => (1 * (s IDbwbse_sort_ch) + -255 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 47%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -255 <= 0)%Z
    | 48%positive => (1 * (s IDbwbse_sort_ch) + -255 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 49%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDbwbse_sort_ch) + 1 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0)%Z
    | 51%positive => (-1 * (s IDbwbse_sort_j) <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ -1 * (s IDbwbse_sort_ch) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDbwbse_sort_ch) + 1 <= 0 /\ 1 * (s IDbwbse_sort_ch) + -256 <= 0 /\ 1 * (s IDbwbse_sort__tmp)+ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0)%Z
    | 55%positive => (-1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0)%Z
    | 57%positive => (-1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_z) <= 0)%Z
    | 58%positive => (-1 * (s IDbwbse_sort_z) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0)%Z
    | 59%positive => (-1 * (s IDbwbse_sort__tmp)+ 1 * (s IDbwbse_sort_j) <= 0 /\ -1 * (s IDbwbse_sort_j) + 1 <= 0 /\ -1 * (s IDbwbse_sort_ch) <= 0 /\ 1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_sum) <= 0 /\ -1 * (s IDbwbse_sort_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition bwbse_sort_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((512 # 1) + (2 # 1) * max0((s IDbwbse_sort_N)))%Q
    | 2%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort_N)))%Q
    | 3%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort_N)))%Q
    | 4%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort_N)))%Q
    | 5%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort_N)))%Q
    | 6%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort_N)))%Q
    | 7%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort__tmp)))%Q
    | 8%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + (2 # 1) * max0((s IDbwbse_sort__tmp)))%Q
    | 9%positive => ((512 # 1) + (s IDbwbse_sort_z)
                     + max0((s IDbwbse_sort__tmp))
                     + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 10%positive => ((512 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 11%positive => ((512 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 12%positive => ((512 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 13%positive => ((512 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 14%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 15%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 16%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 17%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 18%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 19%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 20%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 21%positive => ((256 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 22%positive => ((256 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 23%positive => ((256 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 24%positive => ((256 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 25%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 26%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 27%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 28%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 29%positive => ((s IDbwbse_sort_z))%Q
    | 30%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 31%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 32%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 33%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 34%positive => ((257 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 35%positive => ((257 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 36%positive => ((257 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 37%positive => ((256 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 38%positive => ((256 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 39%positive => ((257 # 1) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0(255 - (s IDbwbse_sort_ch)))%Q
    | 40%positive => ((257 # 1) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0(255 - (s IDbwbse_sort_ch)))%Q
    | 41%positive => ((257 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 42%positive => ((257 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 43%positive => ((257 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 44%positive => ((256 # 1) + (s IDbwbse_sort_z)
                      + max0(255 - (s IDbwbse_sort_ch))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 45%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 46%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 47%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 48%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 49%positive => ((513 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 50%positive => ((513 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 51%positive => ((513 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 52%positive => ((512 # 1) - (s IDbwbse_sort_ch) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 53%positive => ((512 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 54%positive => ((513 # 1) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 55%positive => ((513 # 1) + (s IDbwbse_sort_z)
                      + max0(-1 + (s IDbwbse_sort__tmp) - (s IDbwbse_sort_j))
                      + max0((s IDbwbse_sort__tmp)))%Q
    | 56%positive => ((513 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 57%positive => ((513 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 58%positive => ((513 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | 59%positive => ((512 # 1) + (s IDbwbse_sort_z)
                      + max0((s IDbwbse_sort__tmp))
                      + max0((s IDbwbse_sort__tmp) - (s IDbwbse_sort_j)))%Q
    | _ => (0 # 1)%Q
  end.

Definition bwbse_sort_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDbwbse_sort__tmp)
                                                             - (s IDbwbse_sort_j)) (-1
                                                                    + (s IDbwbse_sort__tmp)
                                                                    - (s IDbwbse_sort_j)))]
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDbwbse_sort__tmp)
                                            - (s IDbwbse_sort_j))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDbwbse_sort_ch)) (255
                                                                    - (s IDbwbse_sort_ch)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - (s IDbwbse_sort_ch)) (0))) (F_max0_ge_0 (256
                                                                    - (s IDbwbse_sort_ch)))]
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_ge_0 (255 - (s IDbwbse_sort_ch))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDbwbse_sort__tmp)
                                                             - (s IDbwbse_sort_j)) (-1
                                                                    + (s IDbwbse_sort__tmp)
                                                                    - (s IDbwbse_sort_j)))]
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDbwbse_sort__tmp)
                                            - (s IDbwbse_sort_j));
                      (*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDbwbse_sort_ch)) (255
                                                                    - (s IDbwbse_sort_ch)));
                      (*-1 0*) F_max0_ge_0 (255 - (s IDbwbse_sort_ch));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - (s IDbwbse_sort_ch)) (0))) (F_max0_ge_0 (256
                                                                    - (s IDbwbse_sort_ch)))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_max0_pre_decrement ((s IDbwbse_sort__tmp)
                                                     - (s IDbwbse_sort_j)) (1)]
    | 39%positive => []
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
    | 53%positive => [(*0 1*) F_max0_pre_decrement ((s IDbwbse_sort__tmp)
                                                    - (s IDbwbse_sort_j)) (1)]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | _ => []
  end.


Theorem bwbse_sort_ai_correct:
  forall s p' s', steps (g_start bwbse_sort) s (g_edges bwbse_sort) p' s' -> bwbse_sort_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bwbse_sort_pot_correct:
  forall s p' s',
    steps (g_start bwbse_sort) s (g_edges bwbse_sort) p' s' ->
    (bwbse_sort_pot (g_start bwbse_sort) s >= bwbse_sort_pot p' s')%Q.
Proof.
  check_lp bwbse_sort_ai_correct bwbse_sort_hints.
Qed.

