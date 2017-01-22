Require Import pasta.Pasta.

Notation IDpdfmark_save_edited_pairs_z := 1%positive.
Notation IDpdfmark_save_edited_pairs__tmp := 2%positive.
Notation IDpdfmark_save_edited_pairs__tmp1 := 3%positive.
Notation IDpdfmark_save_edited_pairs__tmp2 := 4%positive.
Notation IDpdfmark_save_edited_pairs_i := 5%positive.
Notation IDpdfmark_save_edited_pairs_len := 6%positive.
Notation IDpdfmark_save_edited_pairs_len1 := 7%positive.
Notation IDpdfmark_save_edited_pairs_pstr_dref_off8 := 8%positive.
Notation IDpdfmark_save_edited_pairs_size := 9%positive.
Notation IDpdfmark_save_edited_pairs_add_count := 10%positive.
Notation IDpdfmark_save_edited_pairs_add_pairs := 11%positive.
Notation IDpdfmark_save_edited_pairs_count := 12%positive.
Notation IDpdfmark_save_edited_pairs_pairs := 13%positive.
Notation IDpdfmark_save_edited_pairs_pdev := 14%positive.
Notation IDpdfmark_save_edited_pairs_pstr := 15%positive.
Notation IDpdfmark_save_edited_pairs_skip_keys := 16%positive.
Definition pdfmark_save_edited_pairs : graph := {|
  g_start := 1%positive;
  g_end := 62%positive;
  g_edges := (1%positive,(AAssign IDpdfmark_save_edited_pairs_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs__tmp1) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs__tmp) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDpdfmark_save_edited_pairs__tmp
             (Some (EVar IDpdfmark_save_edited_pairs_count))),7%positive)::
             (7%positive,(AAssign IDpdfmark_save_edited_pairs__tmp1
             (Some (EVar IDpdfmark_save_edited_pairs_add_count))),8%positive)::
             (8%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (ENum (0)))),9%positive)::
             (9%positive,(AAssign IDpdfmark_save_edited_pairs_size
             (Some (ENum (0)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) <
             (eval (EVar IDpdfmark_save_edited_pairs__tmp) s))%Z)),
             71%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) >=
             (eval (EVar IDpdfmark_save_edited_pairs__tmp) s))%Z)),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (ENum (0)))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) <
             (eval (EVar IDpdfmark_save_edited_pairs__tmp1) s))%Z)),
             63%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) >=
             (eval (EVar IDpdfmark_save_edited_pairs__tmp1) s))%Z)),
             18%positive)::(18%positive,AWeaken,19%positive)::
             (19%positive,ANone,22%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,24%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,59%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (ENum (0)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) <
             (eval (EVar IDpdfmark_save_edited_pairs__tmp) s))%Z)),
             48%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) >=
             (eval (EVar IDpdfmark_save_edited_pairs__tmp) s))%Z)),
             29%positive)::(29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (ENum (0)))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) <
             (eval (EVar IDpdfmark_save_edited_pairs__tmp1) s))%Z)),
             39%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDpdfmark_save_edited_pairs_i) s) >=
             (eval (EVar IDpdfmark_save_edited_pairs__tmp1) s))%Z)),
             34%positive)::(34%positive,AWeaken,35%positive)::
             (35%positive,(AAssign IDpdfmark_save_edited_pairs_pstr_dref_off8
             (Some (EVar IDpdfmark_save_edited_pairs_size))),36%positive)::
             (36%positive,(AAssign IDpdfmark_save_edited_pairs__tmp2
             (Some (ENum (0)))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,62%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDpdfmark_save_edited_pairs_len1 None),
             41%positive)::
             (41%positive,(AAssign IDpdfmark_save_edited_pairs_len1 None),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (EAdd (EVar IDpdfmark_save_edited_pairs_i) (ENum (2))))),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDpdfmark_save_edited_pairs_z
             (Some (EAdd (ENum (1)) (EVar IDpdfmark_save_edited_pairs_z)))),
             47%positive)::(47%positive,AWeaken,33%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,ANone,53%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDpdfmark_save_edited_pairs_len None),
             51%positive)::
             (51%positive,(AAssign IDpdfmark_save_edited_pairs_len None),
             52%positive)::(52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (EAdd (EVar IDpdfmark_save_edited_pairs_i) (ENum (2))))),
             55%positive)::(55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDpdfmark_save_edited_pairs_z
             (Some (EAdd (ENum (1)) (EVar IDpdfmark_save_edited_pairs_z)))),
             58%positive)::(58%positive,AWeaken,28%positive)::
             (59%positive,(AAssign IDpdfmark_save_edited_pairs__tmp2
             (Some (ENum (-25)))),60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDpdfmark_save_edited_pairs_size None),
             65%positive)::(65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (EAdd (EVar IDpdfmark_save_edited_pairs_i) (ENum (2))))),
             67%positive)::(67%positive,ANone,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDpdfmark_save_edited_pairs_z
             (Some (EAdd (ENum (1)) (EVar IDpdfmark_save_edited_pairs_z)))),
             70%positive)::(70%positive,AWeaken,17%positive)::
             (71%positive,AWeaken,72%positive)::
             (72%positive,ANone,75%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDpdfmark_save_edited_pairs_size None),
             74%positive)::(74%positive,ANone,75%positive)::
             (75%positive,ANone,76%positive)::
             (76%positive,(AAssign IDpdfmark_save_edited_pairs_i
             (Some (EAdd (EVar IDpdfmark_save_edited_pairs_i) (ENum (2))))),
             77%positive)::(77%positive,ANone,78%positive)::
             (78%positive,ANone,79%positive)::
             (79%positive,(AAssign IDpdfmark_save_edited_pairs_z
             (Some (EAdd (ENum (1)) (EVar IDpdfmark_save_edited_pairs_z)))),
             80%positive)::(80%positive,AWeaken,12%positive)::nil
|}.

Definition pdfmark_save_edited_pairs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 4%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1) <= 0)%Z
    | 5%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1) <= 0)%Z
    | 7%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 8%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 10%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_size) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_size) <= 0)%Z
    | 11%positive => (-1 * (s IDpdfmark_save_edited_pairs_size) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_size) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 12%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 13%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 14%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 15%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 16%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 17%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 18%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 19%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 20%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 21%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 22%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 23%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 24%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 25%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 26%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 27%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 28%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 29%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 30%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 31%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 32%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 33%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 34%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 35%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 36%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 37%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp2) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp2) <= 0)%Z
    | 38%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp2) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp2) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 39%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 41%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 43%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0)%Z
    | 45%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 46%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0)%Z
    | 47%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 50%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 52%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 54%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 55%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0)%Z
    | 56%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 57%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0)%Z
    | 58%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) + 1 <= 0)%Z
    | 59%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 60%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp2) + 25 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp2) + -25 <= 0)%Z
    | 61%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp2) + -25 <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp2) + 25 <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 62%positive => (1 * (s IDpdfmark_save_edited_pairs__tmp2) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ 1 * (s IDpdfmark_save_edited_pairs__tmp1)+ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp2) + -25 <= 0)%Z
    | 63%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 65%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0)%Z
    | 67%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0)%Z
    | 68%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 69%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0)%Z
    | 70%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp1)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 73%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 75%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0)%Z
    | 76%positive => (-1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + 1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 77%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0)%Z
    | 78%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) <= 0)%Z
    | 79%positive => (-1 * (s IDpdfmark_save_edited_pairs_z) <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0)%Z
    | 80%positive => (-1 * (s IDpdfmark_save_edited_pairs_i) + 2 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs__tmp)+ 1 * (s IDpdfmark_save_edited_pairs_i) + -1 <= 0 /\ -1 * (s IDpdfmark_save_edited_pairs_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition pdfmark_save_edited_pairs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 + (s IDpdfmark_save_edited_pairs_add_count))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_count)))%Q
    | 2%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs_add_count))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_count)))%Q
    | 3%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs_add_count))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_count)))%Q
    | 4%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs_add_count))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_count)))%Q
    | 5%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs_add_count))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_count)))%Q
    | 6%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs_add_count))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_count)))%Q
    | 7%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs__tmp))
                     + max0(1 + (s IDpdfmark_save_edited_pairs_add_count)))%Q
    | 8%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + max0(1 + (s IDpdfmark_save_edited_pairs__tmp))
                     + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 9%positive => ((s IDpdfmark_save_edited_pairs_z)
                     + (1 # 2) * max0(1
                                      + (s IDpdfmark_save_edited_pairs__tmp))
                     + (1 # 2) * max0(1
                                      + (s IDpdfmark_save_edited_pairs__tmp)
                                      - (s IDpdfmark_save_edited_pairs_i))
                     + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 10%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 11%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 12%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 13%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 14%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 15%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 16%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 17%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 18%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 19%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 20%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 21%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 22%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 23%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 24%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 25%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 26%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 27%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 28%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 29%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 30%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 31%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 32%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 33%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 34%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 35%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 36%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 37%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 38%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 39%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 40%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 41%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 42%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 43%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 44%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 45%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 46%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 47%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 48%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 49%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 50%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 51%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 52%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 53%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 54%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 55%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 56%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 57%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 58%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 59%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 60%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 61%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 62%positive => ((s IDpdfmark_save_edited_pairs_z))%Q
    | 63%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 64%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 65%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 66%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 67%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 68%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 69%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 70%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                       - (s IDpdfmark_save_edited_pairs_i)))%Q
    | 71%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 72%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 73%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 74%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 75%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 76%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(-1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 77%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 78%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 79%positive => ((1 # 1) + (s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | 80%positive => ((s IDpdfmark_save_edited_pairs_z)
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp))
                      + (1 # 2) * max0(1
                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                       - (s IDpdfmark_save_edited_pairs_i))
                      + max0(1 + (s IDpdfmark_save_edited_pairs__tmp1)))%Q
    | _ => (0 # 1)%Q
  end.

Definition pdfmark_save_edited_pairs_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_save_edited_pairs__tmp)
                                                               - (s IDpdfmark_save_edited_pairs_i)) (-1
                                                                    + (s IDpdfmark_save_edited_pairs__tmp)
                                                                    - (s IDpdfmark_save_edited_pairs_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_save_edited_pairs__tmp)
                                              - (s IDpdfmark_save_edited_pairs_i))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_save_edited_pairs__tmp1)
                                                               - (s IDpdfmark_save_edited_pairs_i)) (-1
                                                                    + (s IDpdfmark_save_edited_pairs__tmp1)
                                                                    - (s IDpdfmark_save_edited_pairs_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_save_edited_pairs__tmp1)
                                              - (s IDpdfmark_save_edited_pairs_i))]
    | 22%positive => []
    | 23%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_save_edited_pairs__tmp1)
                                                               - (s IDpdfmark_save_edited_pairs_i)) (-1
                                                                    + (s IDpdfmark_save_edited_pairs__tmp1)
                                                                    - (s IDpdfmark_save_edited_pairs_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_save_edited_pairs__tmp1)
                                              - (s IDpdfmark_save_edited_pairs_i))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_save_edited_pairs__tmp)
                                                               - (s IDpdfmark_save_edited_pairs_i)) (-1
                                                                    + (s IDpdfmark_save_edited_pairs__tmp)
                                                                    - (s IDpdfmark_save_edited_pairs_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_save_edited_pairs__tmp)
                                              - (s IDpdfmark_save_edited_pairs_i))]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDpdfmark_save_edited_pairs__tmp1)
                                                               - (s IDpdfmark_save_edited_pairs_i)) (-1
                                                                    + (s IDpdfmark_save_edited_pairs__tmp1)
                                                                    - (s IDpdfmark_save_edited_pairs_i)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDpdfmark_save_edited_pairs__tmp1)
                                              - (s IDpdfmark_save_edited_pairs_i))]
    | 39%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                                       - (s IDpdfmark_save_edited_pairs_i)) (2)]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                                       - (s IDpdfmark_save_edited_pairs_i)) (2)]
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
    | 61%positive => [(*-0.5 0*) F_max0_ge_0 (1
                                              + (s IDpdfmark_save_edited_pairs__tmp));
                      (*-0.5 0*) F_max0_ge_0 (1
                                              + (s IDpdfmark_save_edited_pairs__tmp1))]
    | 62%positive => []
    | 63%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDpdfmark_save_edited_pairs__tmp1)
                                                       - (s IDpdfmark_save_edited_pairs_i)) (2)]
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDpdfmark_save_edited_pairs__tmp)
                                                       - (s IDpdfmark_save_edited_pairs_i)) (2)]
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | _ => []
  end.


Theorem pdfmark_save_edited_pairs_ai_correct:
  forall s p' s', steps (g_start pdfmark_save_edited_pairs) s (g_edges pdfmark_save_edited_pairs) p' s' -> pdfmark_save_edited_pairs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pdfmark_save_edited_pairs_pot_correct:
  forall s p' s',
    steps (g_start pdfmark_save_edited_pairs) s (g_edges pdfmark_save_edited_pairs) p' s' ->
    (pdfmark_save_edited_pairs_pot (g_start pdfmark_save_edited_pairs) s >= pdfmark_save_edited_pairs_pot p' s')%Q.
Proof.
  check_lp pdfmark_save_edited_pairs_ai_correct pdfmark_save_edited_pairs_hints.
Qed.

