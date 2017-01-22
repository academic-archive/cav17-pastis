Require Import pasta.Pasta.

Notation IDmem_mapped_map_rgb_color_z := 1%positive.
Notation IDmem_mapped_map_rgb_color__tmp := 2%positive.
Notation IDmem_mapped_map_rgb_color__tmp1 := 3%positive.
Notation IDmem_mapped_map_rgb_color__tmp2 := 4%positive.
Notation IDmem_mapped_map_rgb_color_bb := 5%positive.
Notation IDmem_mapped_map_rgb_color_best := 6%positive.
Notation IDmem_mapped_map_rgb_color_bg := 7%positive.
Notation IDmem_mapped_map_rgb_color_br := 8%positive.
Notation IDmem_mapped_map_rgb_color_cnt := 9%positive.
Notation IDmem_mapped_map_rgb_color_db := 10%positive.
Notation IDmem_mapped_map_rgb_color_dev_dref_off552_off8 := 11%positive.
Notation IDmem_mapped_map_rgb_color_dg := 12%positive.
Notation IDmem_mapped_map_rgb_color_diff := 13%positive.
Notation IDmem_mapped_map_rgb_color_b := 14%positive.
Notation IDmem_mapped_map_rgb_color_dev := 15%positive.
Notation IDmem_mapped_map_rgb_color_g := 16%positive.
Notation IDmem_mapped_map_rgb_color_r := 17%positive.
Definition mem_mapped_map_rgb_color : graph := {|
  g_start := 1%positive;
  g_end := 15%positive;
  g_edges := (1%positive,(AAssign IDmem_mapped_map_rgb_color_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmem_mapped_map_rgb_color__tmp2
             (Some (EVar IDmem_mapped_map_rgb_color_r))),3%positive)::
             (3%positive,(AAssign IDmem_mapped_map_rgb_color__tmp1
             (Some (EVar IDmem_mapped_map_rgb_color_g))),4%positive)::
             (4%positive,(AAssign IDmem_mapped_map_rgb_color__tmp
             (Some (EVar IDmem_mapped_map_rgb_color_b))),5%positive)::
             (5%positive,(AAssign IDmem_mapped_map_rgb_color_br None),
             6%positive)::
             (6%positive,(AAssign IDmem_mapped_map_rgb_color_bg None),
             7%positive)::
             (7%positive,(AAssign IDmem_mapped_map_rgb_color_bb None),
             8%positive)::
             (8%positive,(AAssign IDmem_mapped_map_rgb_color_cnt
             (Some (EVar IDmem_mapped_map_rgb_color_dev_dref_off552_off8))),
             9%positive)::
             (9%positive,(AAssign IDmem_mapped_map_rgb_color_best
             (Some (ENum (768)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDmem_mapped_map_rgb_color_cnt
             (Some (ESub (EVar IDmem_mapped_map_rgb_color_cnt) (ENum (3))))),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDmem_mapped_map_rgb_color_cnt)
             (ENum (3))) s) >= (eval (ENum (0)) s))%Z)),16%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDmem_mapped_map_rgb_color_cnt)
             (ENum (3))) s) < (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDmem_mapped_map_rgb_color_diff None),
             18%positive)::(18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_diff) s) <
             (eval (ENum (0)) s))%Z)),21%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_diff) s) >=
             (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,25%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDmem_mapped_map_rgb_color_diff
             (Some (ESub (ENum (0))
             (EVar IDmem_mapped_map_rgb_color_diff)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_diff) s) <
             (eval (EVar IDmem_mapped_map_rgb_color_best) s))%Z)),
             27%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_diff) s) >=
             (eval (EVar IDmem_mapped_map_rgb_color_best) s))%Z)),
             26%positive)::(26%positive,AWeaken,56%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDmem_mapped_map_rgb_color_dg None),
             29%positive)::(29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_dg) s) <
             (eval (ENum (0)) s))%Z)),32%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_dg) s) >=
             (eval (ENum (0)) s))%Z)),31%positive)::
             (31%positive,AWeaken,35%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDmem_mapped_map_rgb_color_dg
             (Some (ESub (ENum (0)) (EVar IDmem_mapped_map_rgb_color_dg)))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDmem_mapped_map_rgb_color_diff
             (Some (EAdd (EVar IDmem_mapped_map_rgb_color_diff)
             (EVar IDmem_mapped_map_rgb_color_dg)))),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmem_mapped_map_rgb_color_diff)
             (EVar IDmem_mapped_map_rgb_color_dg)) s) <
             (eval (EVar IDmem_mapped_map_rgb_color_best) s))%Z)),
             39%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmem_mapped_map_rgb_color_diff)
             (EVar IDmem_mapped_map_rgb_color_dg)) s) >=
             (eval (EVar IDmem_mapped_map_rgb_color_best) s))%Z)),
             38%positive)::(38%positive,AWeaken,55%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDmem_mapped_map_rgb_color_db None),
             41%positive)::(41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_db) s) <
             (eval (ENum (0)) s))%Z)),44%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EVar IDmem_mapped_map_rgb_color_db) s) >=
             (eval (ENum (0)) s))%Z)),43%positive)::
             (43%positive,AWeaken,47%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDmem_mapped_map_rgb_color_db
             (Some (ESub (ENum (0)) (EVar IDmem_mapped_map_rgb_color_db)))),
             46%positive)::(46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDmem_mapped_map_rgb_color_diff
             (Some (EAdd (EVar IDmem_mapped_map_rgb_color_diff)
             (EVar IDmem_mapped_map_rgb_color_db)))),48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmem_mapped_map_rgb_color_diff)
             (EVar IDmem_mapped_map_rgb_color_db)) s) <
             (eval (EVar IDmem_mapped_map_rgb_color_best) s))%Z)),
             51%positive)::
             (49%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDmem_mapped_map_rgb_color_diff)
             (EVar IDmem_mapped_map_rgb_color_db)) s) >=
             (eval (EVar IDmem_mapped_map_rgb_color_best) s))%Z)),
             50%positive)::(50%positive,AWeaken,54%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AAssign IDmem_mapped_map_rgb_color_best
             (Some (EVar IDmem_mapped_map_rgb_color_diff))),53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDmem_mapped_map_rgb_color_z
             (Some (EAdd (ENum (1)) (EVar IDmem_mapped_map_rgb_color_z)))),
             11%positive)::nil
|}.

Definition mem_mapped_map_rgb_color_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 4%positive => (1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 6%positive => (1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 8%positive => (1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 10%positive => (1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_best) + -768 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best) + 768 <= 0)%Z
    | 11%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 14%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_cnt) + -2 <= 0)%Z
    | 15%positive => (1 * (s IDmem_mapped_map_rgb_color_cnt) + -2 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 16%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 17%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 19%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 20%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0)%Z
    | 21%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 23%positive => (-1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0)%Z
    | 25%positive => (-1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 26%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0)%Z
    | 27%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 29%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 31%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0)%Z
    | 32%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDmem_mapped_map_rgb_color_dg) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 34%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 36%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0)%Z
    | 37%positive => (1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 38%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0)%Z
    | 39%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 41%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 43%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0)%Z
    | 44%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_db) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDmem_mapped_map_rgb_color_db) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 46%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 48%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ -1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 50%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0)%Z
    | 51%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ -1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ 1 * (s IDmem_mapped_map_rgb_color_diff) + 1 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 53%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_best)+ 1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg) <= 0)%Z
    | 54%positive => (1 * (s IDmem_mapped_map_rgb_color_db)+ 1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_db) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 55%positive => (1 * (s IDmem_mapped_map_rgb_color_dg)+ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_dg) <= 0)%Z
    | 56%positive => (-1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | 57%positive => (-1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_diff) <= 0)%Z
    | 58%positive => (-1 * (s IDmem_mapped_map_rgb_color_diff) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_z) <= 0 /\ -1 * (s IDmem_mapped_map_rgb_color_cnt) + 3 <= 0)%Z
    | _ => False
  end.

Definition mem_mapped_map_rgb_color_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 3) * max0(-3
                                    + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 2%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 3%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 4%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 5%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 6%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 7%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 8%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3
                                      + (s IDmem_mapped_map_rgb_color_dev_dref_off552_off8)))%Q
    | 9%positive => ((s IDmem_mapped_map_rgb_color_z)
                     + (1 # 3) * max0(-3 + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 10%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 11%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 12%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 13%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 14%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 15%positive => ((s IDmem_mapped_map_rgb_color_z))%Q
    | 16%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 17%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 18%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 19%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 20%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 21%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 22%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 23%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 24%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 25%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 26%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 27%positive => ((s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0((s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 28%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 29%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 30%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 31%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 32%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 33%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 34%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 35%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 36%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 37%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 38%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 39%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 40%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 41%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 42%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 43%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 44%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 45%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 46%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 47%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 48%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 49%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 50%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 51%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 52%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 53%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 54%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 55%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 56%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 57%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | 58%positive => ((1 # 1) + (s IDmem_mapped_map_rgb_color_z)
                      + (1 # 3) * max0(-3
                                       + (s IDmem_mapped_map_rgb_color_cnt)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mem_mapped_map_rgb_color_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-0.333333 0*) F_max0_monotonic (F_check_ge ((s IDmem_mapped_map_rgb_color_cnt)) (-3
                                                                    + (s IDmem_mapped_map_rgb_color_cnt)));
                      (*-0.333333 0*) F_max0_ge_0 (-3
                                                   + (s IDmem_mapped_map_rgb_color_cnt))]
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
    | 26%positive => [(*-0.333333 0*) F_max0_pre_decrement ((s IDmem_mapped_map_rgb_color_cnt)) (3)]
    | 27%positive => [(*0 0.333333*) F_max0_pre_decrement ((s IDmem_mapped_map_rgb_color_cnt)) (3)]
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
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | _ => []
  end.


Theorem mem_mapped_map_rgb_color_ai_correct:
  forall s p' s', steps (g_start mem_mapped_map_rgb_color) s (g_edges mem_mapped_map_rgb_color) p' s' -> mem_mapped_map_rgb_color_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mem_mapped_map_rgb_color_pot_correct:
  forall s p' s',
    steps (g_start mem_mapped_map_rgb_color) s (g_edges mem_mapped_map_rgb_color) p' s' ->
    (mem_mapped_map_rgb_color_pot (g_start mem_mapped_map_rgb_color) s >= mem_mapped_map_rgb_color_pot p' s')%Q.
Proof.
  check_lp mem_mapped_map_rgb_color_ai_correct mem_mapped_map_rgb_color_hints.
Qed.

