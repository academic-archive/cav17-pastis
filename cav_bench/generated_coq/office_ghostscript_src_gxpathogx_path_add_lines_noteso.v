Require Import pasta.Pasta.

Notation IDgx_path_add_lines_notes_z := 1%positive.
Notation IDgx_path_add_lines_notes__tmp := 2%positive.
Notation IDgx_path_add_lines_notes__tmp1 := 3%positive.
Notation IDgx_path_add_lines_notes__tmp2 := 4%positive.
Notation IDgx_path_add_lines_notes_code := 5%positive.
Notation IDgx_path_add_lines_notes_code1 := 6%positive.
Notation IDgx_path_add_lines_notes_i := 7%positive.
Notation IDgx_path_add_lines_notes_x := 8%positive.
Notation IDgx_path_add_lines_notes_y := 9%positive.
Notation IDgx_path_add_lines_notes_count := 10%positive.
Notation IDgx_path_add_lines_notes_notes := 11%positive.
Notation IDgx_path_add_lines_notes_ppath := 12%positive.
Notation IDgx_path_add_lines_notes_ppts := 13%positive.
Definition gx_path_add_lines_notes : graph := {|
  g_start := 1%positive;
  g_end := 76%positive;
  g_edges := (1%positive,(AAssign IDgx_path_add_lines_notes_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgx_path_add_lines_notes__tmp
             (Some (EVar IDgx_path_add_lines_notes_count))),3%positive)::
             (3%positive,(AAssign IDgx_path_add_lines_notes__tmp2
             (Some (EVar IDgx_path_add_lines_notes_notes))),4%positive)::
             (4%positive,(AAssign IDgx_path_add_lines_notes_code
             (Some (ENum (0)))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgx_path_add_lines_notes__tmp) s) <=
             (eval (ENum (0)) s))%Z)),72%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDgx_path_add_lines_notes__tmp) s) >
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,10%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,17%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,15%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDgx_path_add_lines_notes__tmp1
             (Some (ENum (-25)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,76%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,32%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,23%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDgx_path_add_lines_notes__tmp1
             (Some (ENum (-14)))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,76%positive)::
             (23%positive,(AAssign IDgx_path_add_lines_notes_code1 None),
             24%positive)::(24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDgx_path_add_lines_notes_code1) s) <
             (eval (ENum (0)) s))%Z)),28%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDgx_path_add_lines_notes_code1) s) >=
             (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,32%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDgx_path_add_lines_notes__tmp1
             (Some (EVar IDgx_path_add_lines_notes_code1))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,76%positive)::
             (32%positive,(AAssign IDgx_path_add_lines_notes_i
             (Some (ENum (0)))),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDgx_path_add_lines_notes_i) s) <
             (eval (EVar IDgx_path_add_lines_notes__tmp) s))%Z)),37%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDgx_path_add_lines_notes_i) s) >=
             (eval (EVar IDgx_path_add_lines_notes__tmp) s))%Z)),36%positive)::
             (36%positive,AWeaken,67%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDgx_path_add_lines_notes_x None),
             39%positive)::
             (39%positive,(AAssign IDgx_path_add_lines_notes_y None),
             40%positive)::(40%positive,AWeaken,41%positive)::
             (41%positive,ANone,43%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,52%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,64%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,64%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,64%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,ANone,64%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,56%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDgx_path_add_lines_notes_code
             (Some (ENum (-25)))),54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,67%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,(AAssign IDgx_path_add_lines_notes_i
             (Some (EAdd (EVar IDgx_path_add_lines_notes_i) (ENum (1))))),
             60%positive)::(60%positive,ANone,61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,(AAssign IDgx_path_add_lines_notes_z
             (Some (EAdd (ENum (1)) (EVar IDgx_path_add_lines_notes_z)))),
             63%positive)::(63%positive,AWeaken,35%positive)::
             (64%positive,(AAssign IDgx_path_add_lines_notes_code
             (Some (ENum (-15)))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,ANone,68%positive)::
             (67%positive,ANone,69%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDgx_path_add_lines_notes__tmp1
             (Some (EVar IDgx_path_add_lines_notes_code))),70%positive)::
             (70%positive,ANone,71%positive)::
             (71%positive,AWeaken,76%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,(AAssign IDgx_path_add_lines_notes__tmp1
             (Some (ENum (0)))),74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,AWeaken,76%positive)::nil
|}.

Definition gx_path_add_lines_notes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 4%positive => (1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 8%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 12%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) + 25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp1) + -25 <= 0)%Z
    | 14%positive => (-1 * (s IDgx_path_add_lines_notes__tmp1) + -25 <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) + 25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 17%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 19%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 21%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) + 14 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp1) + -14 <= 0)%Z
    | 22%positive => (-1 * (s IDgx_path_add_lines_notes__tmp1) + -14 <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) + 14 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 23%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 24%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 26%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code1) <= 0)%Z
    | 27%positive => (-1 * (s IDgx_path_add_lines_notes_code1) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 28%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code1) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDgx_path_add_lines_notes_code1) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 30%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code1) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDgx_path_add_lines_notes__tmp1) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code1) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 32%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 34%positive => (-1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 36%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp)+ -1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 37%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 39%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 41%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 43%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 44%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 46%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 48%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 50%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 52%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 54%positive => (-1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) + 25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0)%Z
    | 55%positive => (-1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) + 25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 56%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 57%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 59%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0)%Z
    | 60%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 61%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 62%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 63%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 65%positive => (-1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) + 15 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -15 <= 0)%Z
    | 66%positive => (-1 * (s IDgx_path_add_lines_notes_code) + -15 <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) + 15 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0)%Z
    | 67%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0)%Z
    | 69%positive => (1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 70%positive => (-1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp1) + -25 <= 0)%Z
    | 71%positive => (-1 * (s IDgx_path_add_lines_notes__tmp1) + -25 <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp)+ 1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_i) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp) <= 0)%Z
    | 73%positive => (1 * (s IDgx_path_add_lines_notes__tmp) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 74%positive => (-1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) <= 0 /\ -1 * (s IDgx_path_add_lines_notes__tmp1) <= 0)%Z
    | 75%positive => (-1 * (s IDgx_path_add_lines_notes__tmp1) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0)%Z
    | 76%positive => (-1 * (s IDgx_path_add_lines_notes_code) + -25 <= 0 /\ -1 * (s IDgx_path_add_lines_notes_z) <= 0 /\ 1 * (s IDgx_path_add_lines_notes_code) <= 0 /\ 1 * (s IDgx_path_add_lines_notes__tmp1) <= 0)%Z
    | _ => False
  end.

Definition gx_path_add_lines_notes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgx_path_add_lines_notes_count)))%Q
    | 2%positive => ((s IDgx_path_add_lines_notes_z)
                     + max0((s IDgx_path_add_lines_notes_count)))%Q
    | 3%positive => ((s IDgx_path_add_lines_notes_z)
                     + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 4%positive => ((s IDgx_path_add_lines_notes_z)
                     + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 5%positive => ((s IDgx_path_add_lines_notes_z)
                     + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 6%positive => ((s IDgx_path_add_lines_notes_z)
                     + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 7%positive => ((s IDgx_path_add_lines_notes_z)
                     + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 8%positive => ((s IDgx_path_add_lines_notes__tmp)
                     + (s IDgx_path_add_lines_notes_z))%Q
    | 9%positive => ((s IDgx_path_add_lines_notes__tmp)
                     + (s IDgx_path_add_lines_notes_z))%Q
    | 10%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 11%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 12%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 13%positive => (-(1 # 1) + (s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z)
                      + (1 # 11) * max0(-14
                                        - (s IDgx_path_add_lines_notes__tmp1)))%Q
    | 14%positive => (-(1 # 1) + (s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z)
                      + (1 # 11) * max0(-14
                                        - (s IDgx_path_add_lines_notes__tmp1)))%Q
    | 15%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 16%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 17%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 18%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 19%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 20%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 21%positive => (-(1 # 1) + (s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z)
                      + (1 # 13) * max0(-1
                                        - (s IDgx_path_add_lines_notes__tmp1)))%Q
    | 22%positive => (-(1 # 1) + (s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z)
                      + (1 # 13) * max0(-1
                                        - (s IDgx_path_add_lines_notes__tmp1)))%Q
    | 23%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 24%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 25%positive => ((s IDgx_path_add_lines_notes__tmp))%Q
    | 26%positive => ((s IDgx_path_add_lines_notes__tmp))%Q
    | 27%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 28%positive => ((s IDgx_path_add_lines_notes__tmp))%Q
    | 29%positive => ((s IDgx_path_add_lines_notes__tmp))%Q
    | 30%positive => ((s IDgx_path_add_lines_notes__tmp))%Q
    | 31%positive => ((s IDgx_path_add_lines_notes__tmp))%Q
    | 32%positive => ((s IDgx_path_add_lines_notes__tmp)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 33%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 34%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 35%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 36%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 37%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 38%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 39%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 40%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 41%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 42%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 43%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 44%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 45%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 46%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 47%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 48%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 49%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 50%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 51%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 52%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 53%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 54%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i))
                      + (1 # 24) * max0(-1
                                        - (s IDgx_path_add_lines_notes_code)))%Q
    | 55%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i))
                      + (1 # 24) * max0(-1
                                        - (s IDgx_path_add_lines_notes_code)))%Q
    | 56%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 57%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 58%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 59%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0(-1 + (s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 60%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 61%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 62%positive => ((1 # 1) + (s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 63%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)
                             - (s IDgx_path_add_lines_notes_i)))%Q
    | 64%positive => ((s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z))%Q
    | 65%positive => (-(1 # 1) + (s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z)
                      + max0(-14 - (s IDgx_path_add_lines_notes_code)))%Q
    | 66%positive => (-(1 # 1) + (s IDgx_path_add_lines_notes__tmp)
                      - (s IDgx_path_add_lines_notes_i)
                      + (s IDgx_path_add_lines_notes_z)
                      + max0(-14 - (s IDgx_path_add_lines_notes_code)))%Q
    | 67%positive => ((s IDgx_path_add_lines_notes_z))%Q
    | 68%positive => ((s IDgx_path_add_lines_notes_z))%Q
    | 69%positive => ((s IDgx_path_add_lines_notes_z))%Q
    | 70%positive => ((s IDgx_path_add_lines_notes_z))%Q
    | 71%positive => ((s IDgx_path_add_lines_notes_z))%Q
    | 72%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 73%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 74%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 75%positive => ((s IDgx_path_add_lines_notes_z)
                      + max0((s IDgx_path_add_lines_notes__tmp)))%Q
    | 76%positive => ((s IDgx_path_add_lines_notes_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_path_add_lines_notes_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_path_add_lines_notes__tmp))) (F_check_ge ((s IDgx_path_add_lines_notes__tmp)) (0))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgx_path_add_lines_notes__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)));
                      (*-0.0909091 0*) F_binom_monotonic 1 (F_max0_ge_0 (-14
                                                                    - (s IDgx_path_add_lines_notes__tmp1))) (F_check_ge (0) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDgx_path_add_lines_notes__tmp1))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgx_path_add_lines_notes__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)))]
    | 23%positive => []
    | 24%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgx_path_add_lines_notes_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_path_add_lines_notes_z)) (0))) (F_max0_ge_0 ((s IDgx_path_add_lines_notes_z)))]
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgx_path_add_lines_notes_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgx_path_add_lines_notes_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgx_path_add_lines_notes_z)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgx_path_add_lines_notes_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgx_path_add_lines_notes_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgx_path_add_lines_notes_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgx_path_add_lines_notes__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)) (0))) (F_max0_ge_0 ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)))]
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgx_path_add_lines_notes__tmp)
                                                             - (s IDgx_path_add_lines_notes_i)) (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgx_path_add_lines_notes__tmp)
                                            - (s IDgx_path_add_lines_notes_i))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_path_add_lines_notes__tmp)
                                                                  - (s IDgx_path_add_lines_notes_i))) (F_check_ge ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)) (0))]
    | 41%positive => []
    | 42%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)))]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_max0_pre_decrement ((s IDgx_path_add_lines_notes__tmp)
                                                     - (s IDgx_path_add_lines_notes_i)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)) (0))) (F_max0_ge_0 ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgx_path_add_lines_notes__tmp)
                                            - (s IDgx_path_add_lines_notes_i));
                      (*-0.0416667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDgx_path_add_lines_notes_code))) (F_check_ge (0) (0))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-1 0*) F_max0_pre_decrement ((s IDgx_path_add_lines_notes__tmp)
                                                     - (s IDgx_path_add_lines_notes_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgx_path_add_lines_notes__tmp)
                                            - (s IDgx_path_add_lines_notes_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)) (0))) (F_max0_ge_0 ((s IDgx_path_add_lines_notes__tmp)
                                                                    - (s IDgx_path_add_lines_notes_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-14
                                                                 - (s IDgx_path_add_lines_notes_code))) (F_check_ge (0) (0))]
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*-1 0*) F_max0_ge_0 ((s IDgx_path_add_lines_notes__tmp))]
    | 76%positive => []
    | _ => []
  end.


Theorem gx_path_add_lines_notes_ai_correct:
  forall s p' s', steps (g_start gx_path_add_lines_notes) s (g_edges gx_path_add_lines_notes) p' s' -> gx_path_add_lines_notes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_path_add_lines_notes_pot_correct:
  forall s p' s',
    steps (g_start gx_path_add_lines_notes) s (g_edges gx_path_add_lines_notes) p' s' ->
    (gx_path_add_lines_notes_pot (g_start gx_path_add_lines_notes) s >= gx_path_add_lines_notes_pot p' s')%Q.
Proof.
  check_lp gx_path_add_lines_notes_ai_correct gx_path_add_lines_notes_hints.
Qed.

