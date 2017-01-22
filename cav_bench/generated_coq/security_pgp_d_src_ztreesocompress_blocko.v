Require Import pasta.Pasta.

Notation IDcompress_block_z := 1%positive.
Notation IDcompress_block_code := 2%positive.
Notation IDcompress_block_dist := 3%positive.
Notation IDcompress_block_dx := 4%positive.
Notation IDcompress_block_extra := 5%positive.
Notation IDcompress_block_flag := 6%positive.
Notation IDcompress_block_fx := 7%positive.
Notation IDcompress_block_last_lit := 8%positive.
Notation IDcompress_block_lc := 9%positive.
Notation IDcompress_block_lx := 10%positive.
Notation IDcompress_block_dtree := 11%positive.
Notation IDcompress_block_ltree := 12%positive.
Definition compress_block : graph := {|
  g_start := 1%positive;
  g_end := 57%positive;
  g_edges := (1%positive,(AAssign IDcompress_block_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDcompress_block_lx)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_last_lit) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_dist) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDcompress_block_lx (Some (ENum (0)))),
             7%positive)::
             (7%positive,(AAssign IDcompress_block_dx (Some (ENum (0)))),
             8%positive)::
             (8%positive,(AAssign IDcompress_block_fx (Some (ENum (0)))),
             9%positive)::
             (9%positive,(AAssign IDcompress_block_flag (Some (ENum (0)))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_last_lit) s) <>
             (eval (ENum (0)) s))%Z)),13%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_last_lit) s) =
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,57%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,20%positive)::
             (17%positive,(AAssign IDcompress_block_fx
             (Some (EAdd (EVar IDcompress_block_fx) (ENum (1))))),
             18%positive)::
             (18%positive,(AAssign IDcompress_block_flag None),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDcompress_block_lx
             (Some (EAdd (EVar IDcompress_block_lx) (ENum (1))))),
             21%positive)::
             (21%positive,(AAssign IDcompress_block_lc None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,49%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDcompress_block_code None),25%positive)::
             (25%positive,(AAssign IDcompress_block_extra None),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_extra) s) <>
             (eval (ENum (0)) s))%Z)),29%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_extra) s) =
             (eval (ENum (0)) s))%Z)),28%positive)::
             (28%positive,AWeaken,32%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDcompress_block_lc None),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDcompress_block_dx
             (Some (EAdd (EVar IDcompress_block_dx) (ENum (1))))),
             33%positive)::
             (33%positive,(AAssign IDcompress_block_dist None),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_dist) s) <
             (eval (ENum (256)) s))%Z)),38%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_dist) s) >=
             (eval (ENum (256)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,40%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDcompress_block_code None),41%positive)::
             (41%positive,(AAssign IDcompress_block_extra None),42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_extra) s) <>
             (eval (ENum (0)) s))%Z)),45%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDcompress_block_extra) s) =
             (eval (ENum (0)) s))%Z)),44%positive)::
             (44%positive,AWeaken,48%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AAssign IDcompress_block_dist None),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,50%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDcompress_block_flag None),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDcompress_block_lx)
             s) < (eval (EVar IDcompress_block_last_lit) s))%Z)),58%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDcompress_block_lx)
             s) >= (eval (EVar IDcompress_block_last_lit) s))%Z)),
             54%positive)::(54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,AWeaken,57%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDcompress_block_z (Some (EAdd (ENum (1))
             (EVar IDcompress_block_z)))),61%positive)::
             (61%positive,AWeaken,16%positive)::nil
|}.

Definition compress_block_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0)%Z
    | 4%positive => (-1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0)%Z
    | 5%positive => (-1 * (s IDcompress_block_last_lit) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0)%Z
    | 6%positive => (-1 * (s IDcompress_block_dist) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0)%Z
    | 7%positive => (-1 * (s IDcompress_block_last_lit) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0)%Z
    | 8%positive => (-1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 9%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0)%Z
    | 10%positive => (-1 * (s IDcompress_block_fx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_flag) <= 0)%Z
    | 11%positive => (-1 * (s IDcompress_block_flag) <= 0 /\ 1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0)%Z
    | 12%positive => (-1 * (s IDcompress_block_fx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_last_lit) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_flag) <= 0 /\ 1 * (s IDcompress_block_last_lit) <= 0)%Z
    | 13%positive => (-1 * (s IDcompress_block_fx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_last_lit) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDcompress_block_last_lit) + 1 <= 0 /\ -1 * (s IDcompress_block_flag) <= 0 /\ 1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0)%Z
    | 15%positive => (-1 * (s IDcompress_block_fx) <= 0 /\ 1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) <= 0 /\ 1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ 1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_flag) <= 0 /\ -1 * (s IDcompress_block_last_lit) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0)%Z
    | 18%positive => (-1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDcompress_block_fx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0)%Z
    | 20%positive => (-1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 22%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 23%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 24%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 25%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 26%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 27%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 28%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ 1 * (s IDcompress_block_extra) <= 0 /\ -1 * (s IDcompress_block_extra) <= 0)%Z
    | 29%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 30%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 31%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 32%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 33%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 35%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_dist) + 256 <= 0)%Z
    | 37%positive => (-1 * (s IDcompress_block_dist) + 256 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_dist) + -255 <= 0)%Z
    | 39%positive => (1 * (s IDcompress_block_dist) + -255 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 41%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 43%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_extra) <= 0 /\ -1 * (s IDcompress_block_extra) <= 0)%Z
    | 45%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 46%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDcompress_block_dx) + 1 <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 48%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 50%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 51%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 52%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0)%Z
    | 53%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 54%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_last_lit)+ -1 * (s IDcompress_block_lx) <= 0)%Z
    | 55%positive => (1 * (s IDcompress_block_last_lit)+ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 56%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_last_lit)+ -1 * (s IDcompress_block_lx) <= 0)%Z
    | 57%positive => (-1 * (s IDcompress_block_lx) <= 0 /\ 1 * (s IDcompress_block_last_lit)+ -1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 58%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0)%Z
    | 60%positive => (-1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDcompress_block_last_lit)+ 1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_lx) + 1 <= 0 /\ -1 * (s IDcompress_block_fx) <= 0 /\ -1 * (s IDcompress_block_dx) <= 0 /\ -1 * (s IDcompress_block_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition compress_block_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 2%positive => (max0(-1 + (s IDcompress_block_last_lit))
                     + max0((s IDcompress_block_z)))%Q
    | 3%positive => (max0(-1 + (s IDcompress_block_last_lit))
                     + max0((s IDcompress_block_z)))%Q
    | 4%positive => (max0(-1 + (s IDcompress_block_last_lit))
                     + max0((s IDcompress_block_z)))%Q
    | 5%positive => (max0(-1 + (s IDcompress_block_last_lit))
                     + max0((s IDcompress_block_z)))%Q
    | 6%positive => ((s IDcompress_block_z)
                     + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 7%positive => ((s IDcompress_block_z)
                     + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 8%positive => ((s IDcompress_block_z)
                     + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 9%positive => ((s IDcompress_block_z)
                     + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 10%positive => ((s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 11%positive => ((s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 12%positive => ((s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 13%positive => ((s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 14%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 15%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 16%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 17%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 18%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 19%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 20%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 21%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 22%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 23%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 24%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 25%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 26%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 27%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 28%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 29%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 30%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 31%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 32%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 33%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 34%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 35%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 36%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 37%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 38%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 39%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 40%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 41%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 42%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 43%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 44%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 45%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 46%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 47%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 48%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 49%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 50%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 51%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 52%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 53%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 54%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 55%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 56%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 57%positive => ((s IDcompress_block_z))%Q
    | 58%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 59%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 60%positive => ((1 # 1) - (s IDcompress_block_lx)
                      + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | 61%positive => (-(s IDcompress_block_lx) + (s IDcompress_block_z)
                      + max0(-1 + (s IDcompress_block_last_lit)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compress_block_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcompress_block_z))) (F_check_ge ((s IDcompress_block_z)) (0))]
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDcompress_block_last_lit))) (F_check_ge (0) (0))]
    | 13%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcompress_block_lx))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompress_block_lx)) (0))) (F_max0_ge_0 ((s IDcompress_block_lx)))]
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
    | 38%positive => []
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
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcompress_block_last_lit)
                                                             - (s IDcompress_block_lx)) (-1
                                                                    + (s IDcompress_block_last_lit)
                                                                    - (s IDcompress_block_lx)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcompress_block_last_lit)
                                            - (s IDcompress_block_lx));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompress_block_last_lit)
                                                                    - (s IDcompress_block_lx)) (0))) (F_max0_ge_0 ((s IDcompress_block_last_lit)
                                                                    - (s IDcompress_block_lx)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDcompress_block_last_lit))) (F_check_ge (-1
                                                                    + (s IDcompress_block_last_lit)) (0))]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | _ => []
  end.


Theorem compress_block_ai_correct:
  forall s p' s', steps (g_start compress_block) s (g_edges compress_block) p' s' -> compress_block_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compress_block_pot_correct:
  forall s p' s',
    steps (g_start compress_block) s (g_edges compress_block) p' s' ->
    (compress_block_pot (g_start compress_block) s >= compress_block_pot p' s')%Q.
Proof.
  check_lp compress_block_ai_correct compress_block_hints.
Qed.

