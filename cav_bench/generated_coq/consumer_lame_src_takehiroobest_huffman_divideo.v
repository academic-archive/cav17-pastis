Require Import pasta.Pasta.

Notation IDbest_huffman_divide_z := 1%positive.
Notation IDbest_huffman_divide__tmp := 2%positive.
Notation IDbest_huffman_divide__tmp1 := 3%positive.
Notation IDbest_huffman_divide_a1 := 4%positive.
Notation IDbest_huffman_divide_a2 := 5%positive.
Notation IDbest_huffman_divide_bigv := 6%positive.
Notation IDbest_huffman_divide_r0 := 7%positive.
Notation IDbest_huffman_divide_r1 := 8%positive.
Notation IDbest_huffman_divide_ch := 9%positive.
Notation IDbest_huffman_divide_gi := 10%positive.
Notation IDbest_huffman_divide_gr := 11%positive.
Notation IDbest_huffman_divide_ix := 12%positive.
Definition best_huffman_divide : graph := {|
  g_start := 1%positive;
  g_end := 67%positive;
  g_edges := (1%positive,(AAssign IDbest_huffman_divide_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDbest_huffman_divide__tmp1
             (Some (EVar IDbest_huffman_divide_gr))),3%positive)::
             (3%positive,(AAssign IDbest_huffman_divide__tmp
             (Some (EVar IDbest_huffman_divide_ch))),4%positive)::
             (4%positive,(AAssign IDbest_huffman_divide_bigv None),
             5%positive)::
             (5%positive,(AAssign IDbest_huffman_divide_r0
             (Some (ENum (2)))),6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r0) s) <
             (eval (ENum (23)) s))%Z)),10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r0) s) >=
             (eval (ENum (23)) s))%Z)),9%positive)::
             (9%positive,AWeaken,23%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDbest_huffman_divide_a2 None),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_a2) s) >
             (eval (EVar IDbest_huffman_divide_bigv) s))%Z)),21%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_a2) s) <=
             (eval (EVar IDbest_huffman_divide_bigv) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDbest_huffman_divide_r0
             (Some (EAdd (EVar IDbest_huffman_divide_r0) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDbest_huffman_divide_z
             (Some (EAdd (ENum (1)) (EVar IDbest_huffman_divide_z)))),
             20%positive)::(20%positive,AWeaken,8%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r0) s) <=
             (eval (ENum (24)) s))%Z)),68%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r0) s) >
             (eval (ENum (24)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDbest_huffman_divide_r0
             (Some (ENum (0)))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r0) s) <
             (eval (ENum (16)) s))%Z)),32%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r0) s) >=
             (eval (ENum (16)) s))%Z)),31%positive)::
             (31%positive,AWeaken,67%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDbest_huffman_divide_a1 None),
             34%positive)::(34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_a1) s) >
             (eval (EVar IDbest_huffman_divide_bigv) s))%Z)),64%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_a1) s) <=
             (eval (EVar IDbest_huffman_divide_bigv) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,62%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDbest_huffman_divide_r1
             (Some (ENum (0)))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r1) s) <
             (eval (ENum (8)) s))%Z)),49%positive)::
             (41%positive,(AGuard
             (fun s => ((eval (EVar IDbest_huffman_divide_r1) s) >=
             (eval (ENum (8)) s))%Z)),42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDbest_huffman_divide_r0
             (Some (EAdd (EVar IDbest_huffman_divide_r0) (ENum (1))))),
             45%positive)::(45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDbest_huffman_divide_z
             (Some (EAdd (ENum (1)) (EVar IDbest_huffman_divide_z)))),
             48%positive)::(48%positive,AWeaken,30%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,56%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDbest_huffman_divide_a2 None),
             52%positive)::(52%positive,AWeaken,53%positive)::
             (53%positive,ANone,55%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,57%positive)::
             (55%positive,ANone,57%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDbest_huffman_divide_r1
             (Some (EAdd (EVar IDbest_huffman_divide_r1) (ENum (1))))),
             58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDbest_huffman_divide_z
             (Some (EAdd (ENum (1)) (EVar IDbest_huffman_divide_z)))),
             61%positive)::(61%positive,AWeaken,41%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,AWeaken,67%positive)::
             (64%positive,AWeaken,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDbest_huffman_divide_r0
             (Some (EAdd (EVar IDbest_huffman_divide_r0) (ENum (1))))),
             71%positive)::(71%positive,ANone,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDbest_huffman_divide_z
             (Some (EAdd (ENum (1)) (EVar IDbest_huffman_divide_z)))),
             74%positive)::(74%positive,AWeaken,25%positive)::nil
|}.

Definition best_huffman_divide_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 4%positive => (1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 6%positive => (1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -2 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0)%Z
    | 7%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 8%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -23 <= 0)%Z
    | 9%positive => (1 * (s IDbest_huffman_divide_r0) + -23 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 23 <= 0)%Z
    | 10%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0)%Z
    | 11%positive => (1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0)%Z
    | 12%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0)%Z
    | 13%positive => (1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0)%Z
    | 14%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ 1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 15%positive => (1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0)%Z
    | 16%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ 1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 17%positive => (1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -23 <= 0)%Z
    | 18%positive => (1 * (s IDbest_huffman_divide_r0) + -23 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 19%positive => (1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -23 <= 0)%Z
    | 20%positive => (1 * (s IDbest_huffman_divide_r0) + -23 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ 1 * (s IDbest_huffman_divide_a2)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_z) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ -1 * (s IDbest_huffman_divide_a2)+ 1 * (s IDbest_huffman_divide_bigv) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDbest_huffman_divide_a2)+ 1 * (s IDbest_huffman_divide_bigv) + 1 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -22 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0)%Z
    | 23%positive => (1 * (s IDbest_huffman_divide_r0) + -23 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 24%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -23 <= 0)%Z
    | 25%positive => (-1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -25 <= 0)%Z
    | 26%positive => (1 * (s IDbest_huffman_divide_r0) + -25 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 25 <= 0)%Z
    | 27%positive => (-1 * (s IDbest_huffman_divide_r0) + 25 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -25 <= 0)%Z
    | 28%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 29%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 30%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 31%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 16 <= 0)%Z
    | 32%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0)%Z
    | 33%positive => (1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 34%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0)%Z
    | 35%positive => (1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 36%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 37%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 38%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 39%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0)%Z
    | 40%positive => (-1 * (s IDbest_huffman_divide_r1) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 41%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0)%Z
    | 42%positive => (1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) + 8 <= 0)%Z
    | 43%positive => (-1 * (s IDbest_huffman_divide_r1) + 8 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0)%Z
    | 44%positive => (1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) + 8 <= 0)%Z
    | 45%positive => (-1 * (s IDbest_huffman_divide_r1) + 8 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDbest_huffman_divide_r0) + 1 <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) + 8 <= 0)%Z
    | 47%positive => (-1 * (s IDbest_huffman_divide_r1) + 8 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDbest_huffman_divide_r0) + 1 <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) + 8 <= 0 /\ -1 * (s IDbest_huffman_divide_z) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -7 <= 0)%Z
    | 50%positive => (1 * (s IDbest_huffman_divide_r1) + -7 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 51%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -7 <= 0)%Z
    | 52%positive => (1 * (s IDbest_huffman_divide_r1) + -7 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 53%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -7 <= 0)%Z
    | 54%positive => (1 * (s IDbest_huffman_divide_r1) + -7 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 55%positive => (1 * (s IDbest_huffman_divide_r1) + -7 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 56%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -7 <= 0)%Z
    | 57%positive => (1 * (s IDbest_huffman_divide_r1) + -7 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r1) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 58%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ -1 * (s IDbest_huffman_divide_r1) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDbest_huffman_divide_r1) + 1 <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 60%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ -1 * (s IDbest_huffman_divide_r1) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDbest_huffman_divide_r1) + 1 <= 0 /\ 1 * (s IDbest_huffman_divide_r1) + -8 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ -1 * (s IDbest_huffman_divide_z) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ 1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0)%Z
    | 63%positive => (1 * (s IDbest_huffman_divide_a1)+ -1 * (s IDbest_huffman_divide_bigv) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 64%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_a1)+ 1 * (s IDbest_huffman_divide_bigv) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDbest_huffman_divide_a1)+ 1 * (s IDbest_huffman_divide_bigv) + 1 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 66%positive => (-1 * (s IDbest_huffman_divide_r0) <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -15 <= 0 /\ -1 * (s IDbest_huffman_divide_a1)+ 1 * (s IDbest_huffman_divide_bigv) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) <= 0)%Z
    | 68%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -24 <= 0)%Z
    | 69%positive => (1 * (s IDbest_huffman_divide_r0) + -24 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 70%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 2 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -24 <= 0)%Z
    | 71%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -25 <= 0)%Z
    | 72%positive => (1 * (s IDbest_huffman_divide_r0) + -25 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ -1 * (s IDbest_huffman_divide_z) <= 0)%Z
    | 73%positive => (-1 * (s IDbest_huffman_divide_z) <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ 1 * (s IDbest_huffman_divide_r0) + -25 <= 0)%Z
    | 74%positive => (1 * (s IDbest_huffman_divide_r0) + -25 <= 0 /\ -1 * (s IDbest_huffman_divide_r0) + 3 <= 0 /\ -1 * (s IDbest_huffman_divide_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition best_huffman_divide_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((167 # 1))%Q
    | 2%positive => ((167 # 1) + (s IDbest_huffman_divide_z))%Q
    | 3%positive => ((167 # 1) + (s IDbest_huffman_divide_z))%Q
    | 4%positive => ((167 # 1) + (s IDbest_huffman_divide_z))%Q
    | 5%positive => ((167 # 1) + (s IDbest_huffman_divide_z))%Q
    | 6%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                     + (s IDbest_huffman_divide_z)
                     - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                     - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 7%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                     + (s IDbest_huffman_divide_z)
                     - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                     - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 8%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                     + (s IDbest_huffman_divide_z)
                     - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                     - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 9%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                     + (s IDbest_huffman_divide_z)
                     - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                     - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 10%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 11%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 12%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 13%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 14%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 15%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 16%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 17%positive => ((17743 # 107)
                      + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 18%positive => ((17743 # 107)
                      + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 19%positive => ((17743 # 107)
                      + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 20%positive => ((17636 # 107)
                      + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 21%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 22%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 23%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 24%positive => ((506 # 3) + (219 # 77) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 25%positive => ((168 # 1) + (340 # 107) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 26%positive => ((168 # 1) + (340 # 107) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 27%positive => ((144 # 1) + (s IDbest_huffman_divide_z))%Q
    | 28%positive => (-(38 # 113) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0))
                      - (353 # 42) * max0(22 - (s IDbest_huffman_divide_r0))
                      + (353 # 42) * max0(23 - (s IDbest_huffman_divide_r0))
                      - (38 # 113) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 29%positive => (-(38 # 113) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0))
                      - (353 # 42) * max0(22 - (s IDbest_huffman_divide_r0))
                      + (353 # 42) * max0(23 - (s IDbest_huffman_divide_r0))
                      - (38 # 113) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 30%positive => ((s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 31%positive => ((s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 32%positive => ((s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 33%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 34%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 35%positive => (-(198 # 7) + (209 # 127) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 7) * max0(22 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 36%positive => (-(198 # 7) + (209 # 127) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 7) * max0(22 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 37%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 38%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 39%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 40%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 41%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 42%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 43%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 44%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 45%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 46%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 47%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 48%positive => ((8 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(16 - (s IDbest_huffman_divide_r0)))%Q
    | 49%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 50%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 51%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 52%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 53%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 54%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 55%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 56%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 57%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 58%positive => ((10 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 59%positive => ((10 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 60%positive => ((10 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 61%positive => ((9 # 1) - (s IDbest_huffman_divide_r1)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0)))%Q
    | 62%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 63%positive => ((9 # 25) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 64%positive => (-(198 # 7) + (209 # 127) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 1) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 7) * max0(22 - (s IDbest_huffman_divide_r0))
                      + (9 # 25) * max0(25 - (s IDbest_huffman_divide_r0)))%Q
    | 65%positive => (-(135 # 7) + (9 # 7) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 7) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 7) * max0(22 - (s IDbest_huffman_divide_r0)))%Q
    | 66%positive => (-(135 # 7) + (9 # 7) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      + (9 # 7) * max0(15 - (s IDbest_huffman_divide_r0))
                      + (9 # 7) * max0(22 - (s IDbest_huffman_divide_r0)))%Q
    | 67%positive => ((s IDbest_huffman_divide_z))%Q
    | 68%positive => ((168 # 1) + (340 # 107) * (s IDbest_huffman_divide_r0)
                      + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (323 # 92) * max0((s IDbest_huffman_divide_r0)))%Q
    | 69%positive => ((168 # 1) + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0((s IDbest_huffman_divide_r0)))%Q
    | 70%positive => ((168 # 1) + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0((s IDbest_huffman_divide_r0)))%Q
    | 71%positive => ((168 # 1) + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-3 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 72%positive => ((168 # 1) + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-3 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 73%positive => ((168 # 1) + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-3 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | 74%positive => ((167 # 1) + (s IDbest_huffman_divide_z)
                      - (1 # 3) * max0(-3 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-2 + (s IDbest_huffman_divide_r0))
                      - (1 # 3) * max0(-1 + (s IDbest_huffman_divide_r0)))%Q
    | _ => (0 # 1)%Q
  end.

Definition best_huffman_divide_hints (p : node) (s : state) := 
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
    | 20%positive => [(*-3.51086 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDbest_huffman_divide_r0))) (F_check_ge ((s IDbest_huffman_divide_r0)) (0));
                      (*-3.17752 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbest_huffman_divide_r0)));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDbest_huffman_divide_r0)))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + (s IDbest_huffman_divide_r0))) (F_check_ge (-2
                                                                    + (s IDbest_huffman_divide_r0)) (0))]
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (25
                                                             - (s IDbest_huffman_divide_r0)) (24
                                                                    - (s IDbest_huffman_divide_r0)));
                      (*-1 0*) F_max0_ge_0 (24 - (s IDbest_huffman_divide_r0));
                      (*-3.51086 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 ((s IDbest_huffman_divide_r0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                                    - (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (25
                                                                    - (s IDbest_huffman_divide_r0)));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDbest_huffman_divide_r0)));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDbest_huffman_divide_r0)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-8.40467 0*) F_max0_pre_decrement (23
                                                           - (s IDbest_huffman_divide_r0)) (1);
                      (*-0.336187 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                                    - (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (25
                                                                    - (s IDbest_huffman_divide_r0)))]
    | 30%positive => []
    | 31%positive => [(*-9 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDbest_huffman_divide_r0)) (15
                                                                    - (s IDbest_huffman_divide_r0)));
                      (*-9 0*) F_max0_ge_0 (15 - (s IDbest_huffman_divide_r0))]
    | 32%positive => [(*0 9*) F_max0_pre_decrement (16
                                                    - (s IDbest_huffman_divide_r0)) (1);
                      (*0 0.36*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                                    - (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (25
                                                                    - (s IDbest_huffman_divide_r0)))]
    | 33%positive => []
    | 34%positive => [(*-1.28571 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (22
                                                                    - (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (22
                                                                    - (s IDbest_huffman_divide_r0)))]
    | 35%positive => []
    | 36%positive => [(*0 1.28571*) F_binom_monotonic 1 (F_max0_ge_arg (22
                                                                    - (s IDbest_huffman_divide_r0))) (F_check_ge (22
                                                                    - (s IDbest_huffman_divide_r0)) (0))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-0.36 0*) F_max0_pre_decrement (25
                                                        - (s IDbest_huffman_divide_r0)) (1);
                      (*-0.36 0*) F_binom_monotonic 1 (F_max0_ge_arg (24
                                                                    - (s IDbest_huffman_divide_r0))) (F_check_ge (24
                                                                    - (s IDbest_huffman_divide_r0)) (0))]
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*0 1*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDbest_huffman_divide_r1)) (7
                                                                    - (s IDbest_huffman_divide_r1)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDbest_huffman_divide_r1)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDbest_huffman_divide_r1)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                 - (s IDbest_huffman_divide_r1))) (F_check_ge (0) (0))]
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
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => [(*-0.36 0*) F_one;
                      (*-0.36 0*) F_max0_pre_decrement (25
                                                        - (s IDbest_huffman_divide_r0)) (1);
                      (*-9 0*) F_max0_ge_0 (15 - (s IDbest_huffman_divide_r0));
                      (*-0.36 0*) F_max0_ge_0 (24
                                               - (s IDbest_huffman_divide_r0));
                      (*-0.36 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDbest_huffman_divide_r0))) (F_check_ge (0) (0));
                      (*-0.36 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 ((s IDbest_huffman_divide_r0)))]
    | 64%positive => [(*0 7.71429*) F_max0_ge_0 (15
                                                 - (s IDbest_huffman_divide_r0));
                      (*0 0.36*) F_binom_monotonic 1 (F_max0_ge_arg (25
                                                                    - 
                                                                    (s IDbest_huffman_divide_r0))) (F_check_ge (25
                                                                    - (s IDbest_huffman_divide_r0)) (0))]
    | 65%positive => []
    | 66%positive => [(*-1.28571 0*) F_max0_ge_0 (22
                                                  - (s IDbest_huffman_divide_r0));
                      (*-1.28571 0*) F_binom_monotonic 1 (F_max0_ge_arg (15
                                                                    - (s IDbest_huffman_divide_r0))) (F_check_ge (15
                                                                    - (s IDbest_huffman_divide_r0)) (0))]
    | 67%positive => []
    | 68%positive => [(*0 3.17752*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 ((s IDbest_huffman_divide_r0)))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => [(*-3.51086 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDbest_huffman_divide_r0))) (F_check_ge ((s IDbest_huffman_divide_r0)) (0));
                      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + (s IDbest_huffman_divide_r0)) (0))) (F_max0_ge_0 (-3
                                                                    + (s IDbest_huffman_divide_r0)))]
    | _ => []
  end.


Theorem best_huffman_divide_ai_correct:
  forall s p' s', steps (g_start best_huffman_divide) s (g_edges best_huffman_divide) p' s' -> best_huffman_divide_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem best_huffman_divide_pot_correct:
  forall s p' s',
    steps (g_start best_huffman_divide) s (g_edges best_huffman_divide) p' s' ->
    (best_huffman_divide_pot (g_start best_huffman_divide) s >= best_huffman_divide_pot p' s')%Q.
Proof.
  check_lp best_huffman_divide_ai_correct best_huffman_divide_hints.
Qed.

