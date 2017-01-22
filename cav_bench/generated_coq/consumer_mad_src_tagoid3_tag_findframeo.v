Require Import pasta.Pasta.

Notation IDid3_tag_findframe_z := 1%positive.
Notation IDid3_tag_findframe__tmp := 2%positive.
Notation IDid3_tag_findframe_i := 3%positive.
Notation IDid3_tag_findframe_len := 4%positive.
Notation IDid3_tag_findframe_tag_dref_off24 := 5%positive.
Notation IDid3_tag_findframe_id := 6%positive.
Notation IDid3_tag_findframe_index := 7%positive.
Notation IDid3_tag_findframe_tag := 8%positive.
Definition id3_tag_findframe : graph := {|
  g_start := 1%positive;
  g_end := 55%positive;
  g_edges := (1%positive,(AAssign IDid3_tag_findframe_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe_tag_dref_off24) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe__tmp) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDid3_tag_findframe__tmp
             (Some (EVar IDid3_tag_findframe_index))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,47%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,46%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDid3_tag_findframe_len None),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe_len) s) =
             (eval (ENum (4)) s))%Z)),15%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe_len) s) <>
             (eval (ENum (4)) s))%Z)),14%positive)::
             (14%positive,AWeaken,24%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,23%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (18%positive,ANone,23%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,23%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDid3_tag_findframe_len None),22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDid3_tag_findframe_i (Some (ENum (0)))),
             25%positive)::(25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe_i) s) <
             (eval (EVar IDid3_tag_findframe_tag_dref_off24) s))%Z)),
             31%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe_i) s) >=
             (eval (EVar IDid3_tag_findframe_tag_dref_off24) s))%Z)),
             28%positive)::(28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,55%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (32%positive,ANone,37%positive)::
             (33%positive,(AAssign IDid3_tag_findframe__tmp
             (Some (EAdd (EVar IDid3_tag_findframe__tmp) (ENum (-1))))),
             34%positive)::(34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe__tmp) s) =
             (eval (ENum (0)) s))%Z)),43%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe__tmp) s) <>
             (eval (ENum (0)) s))%Z)),36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDid3_tag_findframe_i
             (Some (EAdd (EVar IDid3_tag_findframe_i) (ENum (1))))),
             39%positive)::(39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDid3_tag_findframe_z
             (Some (EAdd (ENum (1)) (EVar IDid3_tag_findframe_z)))),
             42%positive)::(42%positive,AWeaken,27%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,55%positive)::
             (46%positive,AWeaken,48%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe__tmp) s) <
             (eval (EVar IDid3_tag_findframe_tag_dref_off24) s))%Z)),
             51%positive)::
             (48%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_findframe__tmp) s) >=
             (eval (EVar IDid3_tag_findframe_tag_dref_off24) s))%Z)),
             49%positive)::(49%positive,AWeaken,50%positive)::
             (50%positive,ANone,53%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,55%positive)::nil
|}.

Definition id3_tag_findframe_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0)%Z
    | 3%positive => (-1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 4%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 5%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDid3_tag_findframe__tmp) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 7%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 8%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 9%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 10%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 11%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 12%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 13%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 14%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 15%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_len) + 4 <= 0)%Z
    | 16%positive => (-1 * (s IDid3_tag_findframe_len) + 4 <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 17%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_len) + 4 <= 0)%Z
    | 18%positive => (-1 * (s IDid3_tag_findframe_len) + 4 <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 19%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_len) + 4 <= 0)%Z
    | 20%positive => (-1 * (s IDid3_tag_findframe_len) + 4 <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 21%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_len) + -4 <= 0 /\ -1 * (s IDid3_tag_findframe_len) + 4 <= 0)%Z
    | 22%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 23%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 24%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 25%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 26%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 27%positive => (-1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 28%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i)+ 1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 29%positive => (-1 * (s IDid3_tag_findframe_i)+ 1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 30%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i)+ 1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 31%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 33%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 35%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 37%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 39%positive => (-1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDid3_tag_findframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0)%Z
    | 41%positive => (-1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDid3_tag_findframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_z) + 1 <= 0)%Z
    | 43%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe__tmp) <= 0 /\ -1 * (s IDid3_tag_findframe__tmp) <= 0)%Z
    | 44%positive => (-1 * (s IDid3_tag_findframe__tmp) <= 0 /\ 1 * (s IDid3_tag_findframe__tmp) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDid3_tag_findframe_i)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0 /\ 1 * (s IDid3_tag_findframe__tmp) <= 0 /\ -1 * (s IDid3_tag_findframe__tmp) <= 0)%Z
    | 46%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 47%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 48%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 49%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe__tmp)+ 1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 50%positive => (-1 * (s IDid3_tag_findframe__tmp)+ 1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 51%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe__tmp)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0)%Z
    | 52%positive => (1 * (s IDid3_tag_findframe__tmp)+ -1 * (s IDid3_tag_findframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 53%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | 54%positive => (-1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_i) <= 0)%Z
    | 55%positive => (-1 * (s IDid3_tag_findframe_i) <= 0 /\ -1 * (s IDid3_tag_findframe_z) <= 0 /\ -1 * (s IDid3_tag_findframe_tag_dref_off24) <= 0)%Z
    | _ => False
  end.

Definition id3_tag_findframe_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 2%positive => ((s IDid3_tag_findframe_z)
                     + max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 3%positive => ((s IDid3_tag_findframe_z)
                     + max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 4%positive => ((s IDid3_tag_findframe_z)
                     + max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 5%positive => ((s IDid3_tag_findframe_z)
                     + max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 6%positive => ((s IDid3_tag_findframe_z)
                     + max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 7%positive => ((s IDid3_tag_findframe_z)
                     + max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 8%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                     + (s IDid3_tag_findframe_z))%Q
    | 9%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                     + (s IDid3_tag_findframe_z))%Q
    | 10%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 11%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 12%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 13%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 14%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 15%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 16%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 17%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 18%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 19%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 20%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 21%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 22%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 23%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 24%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 25%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z)
                      + max0(-(s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24))
                      - max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 26%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z)
                      + max0(-(s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24))
                      - max0((s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 27%positive => (max0(-(s IDid3_tag_findframe_i)
                           + (s IDid3_tag_findframe_tag_dref_off24))
                      + max0((s IDid3_tag_findframe_z)))%Q
    | 28%positive => (max0(-(s IDid3_tag_findframe_i)
                           + (s IDid3_tag_findframe_tag_dref_off24))
                      + max0((s IDid3_tag_findframe_z)))%Q
    | 29%positive => (max0(-(s IDid3_tag_findframe_i)
                           + (s IDid3_tag_findframe_tag_dref_off24))
                      + max0((s IDid3_tag_findframe_z)))%Q
    | 30%positive => (max0(-(s IDid3_tag_findframe_i)
                           + (s IDid3_tag_findframe_tag_dref_off24))
                      + max0((s IDid3_tag_findframe_z)))%Q
    | 31%positive => (max0(-(s IDid3_tag_findframe_i)
                           + (s IDid3_tag_findframe_tag_dref_off24))
                      + max0((s IDid3_tag_findframe_z)))%Q
    | 32%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 33%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 34%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 35%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 36%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 37%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 38%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 39%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-(s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 40%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-(s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 41%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-(s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 42%positive => ((s IDid3_tag_findframe_z)
                      + max0(-(s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 43%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 44%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 45%positive => ((1 # 1) + (s IDid3_tag_findframe_z)
                      + max0(-1 - (s IDid3_tag_findframe_i)
                             + (s IDid3_tag_findframe_tag_dref_off24)))%Q
    | 46%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 47%positive => ((s IDid3_tag_findframe_tag_dref_off24)
                      + (s IDid3_tag_findframe_z))%Q
    | 48%positive => ((s IDid3_tag_findframe_z))%Q
    | 49%positive => ((s IDid3_tag_findframe_z))%Q
    | 50%positive => ((s IDid3_tag_findframe_z))%Q
    | 51%positive => ((s IDid3_tag_findframe_z))%Q
    | 52%positive => ((s IDid3_tag_findframe_z))%Q
    | 53%positive => ((s IDid3_tag_findframe_z))%Q
    | 54%positive => ((s IDid3_tag_findframe_z))%Q
    | 55%positive => ((s IDid3_tag_findframe_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition id3_tag_findframe_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDid3_tag_findframe_tag_dref_off24))) (F_check_ge ((s IDid3_tag_findframe_tag_dref_off24)) (0))]
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
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_findframe_z)) (0))) (F_max0_ge_0 ((s IDid3_tag_findframe_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_findframe_tag_dref_off24)) (0))) (F_max0_ge_0 ((s IDid3_tag_findframe_tag_dref_off24)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDid3_tag_findframe_i)
                                                             + (s IDid3_tag_findframe_tag_dref_off24)) (-1
                                                                    - (s IDid3_tag_findframe_i)
                                                                    + (s IDid3_tag_findframe_tag_dref_off24)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDid3_tag_findframe_i)
                                            + (s IDid3_tag_findframe_tag_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDid3_tag_findframe_z))) (F_check_ge ((s IDid3_tag_findframe_z)) (0))]
    | 31%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDid3_tag_findframe_i)
                                                     + (s IDid3_tag_findframe_tag_dref_off24)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDid3_tag_findframe_z))) (F_check_ge ((s IDid3_tag_findframe_z)) (0))]
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
    | 42%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_findframe_z)) (0))) (F_max0_ge_0 ((s IDid3_tag_findframe_z)))]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDid3_tag_findframe_i)
                                            + (s IDid3_tag_findframe_tag_dref_off24))]
    | 46%positive => [(*-1 0*) F_max0_ge_0 ((s IDid3_tag_findframe_tag_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_findframe_tag_dref_off24)) (0))) (F_max0_ge_0 ((s IDid3_tag_findframe_tag_dref_off24)))]
    | 47%positive => [(*-1 0*) F_max0_ge_0 ((s IDid3_tag_findframe_tag_dref_off24));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_findframe_tag_dref_off24)) (0))) (F_max0_ge_0 ((s IDid3_tag_findframe_tag_dref_off24)))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | _ => []
  end.


Theorem id3_tag_findframe_ai_correct:
  forall s p' s', steps (g_start id3_tag_findframe) s (g_edges id3_tag_findframe) p' s' -> id3_tag_findframe_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem id3_tag_findframe_pot_correct:
  forall s p' s',
    steps (g_start id3_tag_findframe) s (g_edges id3_tag_findframe) p' s' ->
    (id3_tag_findframe_pot (g_start id3_tag_findframe) s >= id3_tag_findframe_pot p' s')%Q.
Proof.
  check_lp id3_tag_findframe_ai_correct id3_tag_findframe_hints.
Qed.

