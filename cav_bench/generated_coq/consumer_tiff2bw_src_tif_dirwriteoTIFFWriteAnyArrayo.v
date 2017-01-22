Require Import pasta.Pasta.

Notation IDTIFFWriteAnyArray_z := 1%positive.
Notation IDTIFFWriteAnyArray__tmp := 2%positive.
Notation IDTIFFWriteAnyArray__tmp1 := 3%positive.
Notation IDTIFFWriteAnyArray__tmp2 := 4%positive.
Notation IDTIFFWriteAnyArray__tmp3 := 5%positive.
Notation IDTIFFWriteAnyArray_i := 6%positive.
Notation IDTIFFWriteAnyArray_status := 7%positive.
Notation IDTIFFWriteAnyArray_dir := 8%positive.
Notation IDTIFFWriteAnyArray_n := 9%positive.
Notation IDTIFFWriteAnyArray_tag := 10%positive.
Notation IDTIFFWriteAnyArray_tif := 11%positive.
Notation IDTIFFWriteAnyArray_type := 12%positive.
Notation IDTIFFWriteAnyArray_v := 13%positive.
Definition TIFFWriteAnyArray : graph := {|
  g_start := 1%positive;
  g_end := 137%positive;
  g_edges := (1%positive,(AAssign IDTIFFWriteAnyArray_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDTIFFWriteAnyArray__tmp3
             (Some (EVar IDTIFFWriteAnyArray_type))),3%positive)::
             (3%positive,(AAssign IDTIFFWriteAnyArray__tmp2
             (Some (EVar IDTIFFWriteAnyArray_tag))),4%positive)::
             (4%positive,(AAssign IDTIFFWriteAnyArray__tmp
             (Some (EVar IDTIFFWriteAnyArray_n))),5%positive)::
             (5%positive,(AAssign IDTIFFWriteAnyArray_status
             (Some (ENum (0)))),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,9%positive)::
             (7%positive,ANone,8%positive)::
             (8%positive,AWeaken,11%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,130%positive)::
             (11%positive,ANone,111%positive)::
             (11%positive,ANone,95%positive)::
             (11%positive,ANone,79%positive)::
             (11%positive,ANone,63%positive)::
             (11%positive,ANone,47%positive)::
             (11%positive,ANone,31%positive)::
             (11%positive,ANone,15%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDTIFFWriteAnyArray__tmp1 None),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,137%positive)::
             (15%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),24%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,23%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,132%positive)::
             (23%positive,ANone,120%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             30%positive)::(30%positive,AWeaken,18%positive)::
             (31%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),40%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,39%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,132%positive)::
             (39%positive,ANone,120%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             43%positive)::(43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             46%positive)::(46%positive,AWeaken,34%positive)::
             (47%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             48%positive)::(48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),56%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,55%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,132%positive)::
             (55%positive,ANone,120%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             59%positive)::(59%positive,ANone,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             62%positive)::(62%positive,AWeaken,50%positive)::
             (63%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             64%positive)::(64%positive,ANone,65%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),72%positive)::
             (66%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),67%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,ANone,71%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,AWeaken,132%positive)::
             (71%positive,ANone,120%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             75%positive)::(75%positive,ANone,76%positive)::
             (76%positive,ANone,77%positive)::
             (77%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             78%positive)::(78%positive,AWeaken,66%positive)::
             (79%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             80%positive)::(80%positive,ANone,81%positive)::
             (81%positive,AWeaken,82%positive)::
             (82%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),88%positive)::
             (82%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),83%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,ANone,87%positive)::
             (84%positive,ANone,85%positive)::
             (85%positive,ANone,86%positive)::
             (86%positive,AWeaken,132%positive)::
             (87%positive,ANone,120%positive)::
             (88%positive,AWeaken,89%positive)::
             (89%positive,ANone,90%positive)::
             (90%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             91%positive)::(91%positive,ANone,92%positive)::
             (92%positive,ANone,93%positive)::
             (93%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             94%positive)::(94%positive,AWeaken,82%positive)::
             (95%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             96%positive)::(96%positive,ANone,97%positive)::
             (97%positive,AWeaken,98%positive)::
             (98%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),104%positive)::
             (98%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),99%positive)::
             (99%positive,AWeaken,100%positive)::
             (100%positive,ANone,103%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,ANone,102%positive)::
             (102%positive,AWeaken,132%positive)::
             (103%positive,ANone,120%positive)::
             (104%positive,AWeaken,105%positive)::
             (105%positive,ANone,106%positive)::
             (106%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             107%positive)::(107%positive,ANone,108%positive)::
             (108%positive,ANone,109%positive)::
             (109%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             110%positive)::(110%positive,AWeaken,98%positive)::
             (111%positive,(AAssign IDTIFFWriteAnyArray_i (Some (ENum (0)))),
             112%positive)::(112%positive,ANone,113%positive)::
             (113%positive,AWeaken,114%positive)::
             (114%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) <
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),123%positive)::
             (114%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFWriteAnyArray_i) s) >=
             (eval (EVar IDTIFFWriteAnyArray__tmp) s))%Z)),115%positive)::
             (115%positive,AWeaken,116%positive)::
             (116%positive,ANone,119%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,ANone,118%positive)::
             (118%positive,AWeaken,132%positive)::
             (119%positive,ANone,120%positive)::
             (120%positive,(AAssign IDTIFFWriteAnyArray_status
             (Some (ENum (1)))),121%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,AWeaken,132%positive)::
             (123%positive,AWeaken,124%positive)::
             (124%positive,ANone,125%positive)::
             (125%positive,(AAssign IDTIFFWriteAnyArray_i
             (Some (EAdd (EVar IDTIFFWriteAnyArray_i) (ENum (1))))),
             126%positive)::(126%positive,ANone,127%positive)::
             (127%positive,ANone,128%positive)::
             (128%positive,(AAssign IDTIFFWriteAnyArray_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFWriteAnyArray_z)))),
             129%positive)::(129%positive,AWeaken,114%positive)::
             (130%positive,ANone,131%positive)::
             (131%positive,AWeaken,132%positive)::
             (132%positive,ANone,133%positive)::
             (132%positive,ANone,134%positive)::
             (133%positive,ANone,134%positive)::
             (134%positive,(AAssign IDTIFFWriteAnyArray__tmp1
             (Some (EVar IDTIFFWriteAnyArray_status))),135%positive)::
             (135%positive,ANone,136%positive)::
             (136%positive,AWeaken,137%positive)::nil
|}.

Definition TIFFWriteAnyArray_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 4%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 6%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 8%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 9%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 10%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 11%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 13%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 16%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 18%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 19%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 20%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 21%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 22%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 23%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 24%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 26%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 28%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 29%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 30%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 32%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 33%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 34%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 35%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 36%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 37%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 38%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 39%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 40%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 42%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 44%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 45%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 46%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 48%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 49%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 50%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 51%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 52%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 53%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 54%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 55%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 56%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 58%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 60%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 61%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 62%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 64%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 65%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 66%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 67%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 68%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 69%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 70%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 71%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 72%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 73%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 74%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 75%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 76%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 77%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 78%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 79%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 80%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 81%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 82%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 83%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 84%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 85%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 86%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 87%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 88%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 89%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 90%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 91%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 92%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 93%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 94%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 95%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 96%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 97%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 98%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 99%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 100%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 101%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 102%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 103%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 104%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 105%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 106%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 107%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 108%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 109%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 110%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 111%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 112%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 113%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 114%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 115%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 116%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 117%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 118%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 119%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 120%positive => (1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 121%positive => (-1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) + 1 <= 0)%Z
    | 122%positive => (-1 * (s IDTIFFWriteAnyArray_status) + 1 <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp)+ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 123%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 124%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 125%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0)%Z
    | 126%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 127%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 128%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0)%Z
    | 129%positive => (-1 * (s IDTIFFWriteAnyArray__tmp)+ 1 * (s IDTIFFWriteAnyArray_i) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_i) + 1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) + 1 <= 0)%Z
    | 130%positive => (-1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 131%positive => (1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0)%Z
    | 132%positive => (1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 133%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0)%Z
    | 134%positive => (1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 135%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp1) + -1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray__tmp1) <= 0)%Z
    | 136%positive => (-1 * (s IDTIFFWriteAnyArray__tmp1) <= 0 /\ 1 * (s IDTIFFWriteAnyArray__tmp1) + -1 <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_z) <= 0)%Z
    | 137%positive => (-1 * (s IDTIFFWriteAnyArray_z) <= 0 /\ -1 * (s IDTIFFWriteAnyArray_status) <= 0 /\ 1 * (s IDTIFFWriteAnyArray_status) + -1 <= 0)%Z
    | _ => False
  end.

Definition TIFFWriteAnyArray_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDTIFFWriteAnyArray_n)))%Q
    | 2%positive => (max0((s IDTIFFWriteAnyArray_n))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 3%positive => (max0((s IDTIFFWriteAnyArray_n))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 4%positive => (max0((s IDTIFFWriteAnyArray_n))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 5%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 6%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 7%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 8%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 9%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                     + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 10%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 11%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 12%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 13%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 14%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 15%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 16%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 17%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 18%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 19%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 20%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 21%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 22%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 23%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 24%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 25%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 26%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 27%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 28%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 29%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 30%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 31%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 32%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 33%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 34%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 35%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 36%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 37%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 38%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 39%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 40%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 41%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 42%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 43%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 44%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 45%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 46%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 47%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 48%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 49%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 50%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 51%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 52%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 53%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 54%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 55%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 56%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 57%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 58%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 59%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 60%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 61%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 62%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 63%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 64%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 65%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 66%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 67%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 68%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 69%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 70%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 71%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 72%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 73%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 74%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 75%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 76%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 77%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 78%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 79%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 80%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 81%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 82%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 83%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 84%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 85%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 86%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 87%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 88%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 89%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 90%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 91%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 92%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 93%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 94%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 95%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 96%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 97%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                           - (s IDTIFFWriteAnyArray_i))
                      + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 98%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 99%positive => ((s IDTIFFWriteAnyArray_z)
                      + max0((s IDTIFFWriteAnyArray__tmp)
                             - (s IDTIFFWriteAnyArray_i)))%Q
    | 100%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 101%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 102%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 103%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 104%positive => ((s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 105%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 106%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 107%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 108%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 109%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 110%positive => ((s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 111%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                       + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 112%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                            - (s IDTIFFWriteAnyArray_i))
                       + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 113%positive => (max0((s IDTIFFWriteAnyArray__tmp)
                            - (s IDTIFFWriteAnyArray_i))
                       + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 114%positive => ((s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 115%positive => ((s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 116%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 117%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 118%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 119%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 120%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 121%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 122%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 123%positive => ((s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 124%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 125%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0(-1 + (s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 126%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 127%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 128%positive => ((1 # 1) + (s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 129%positive => ((s IDTIFFWriteAnyArray_z)
                       + max0((s IDTIFFWriteAnyArray__tmp)
                              - (s IDTIFFWriteAnyArray_i)))%Q
    | 130%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                       + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 131%positive => (max0((s IDTIFFWriteAnyArray__tmp))
                       + max0((s IDTIFFWriteAnyArray_z)))%Q
    | 132%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 133%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 134%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 135%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 136%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | 137%positive => ((s IDTIFFWriteAnyArray_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFWriteAnyArray_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-1 0*) F_max0_ge_0 ((s IDTIFFWriteAnyArray__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                             - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFWriteAnyArray__tmp)
                                            - (s IDTIFFWriteAnyArray_i))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                    - (s IDTIFFWriteAnyArray_i)) (1)]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                             - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFWriteAnyArray__tmp)
                                            - (s IDTIFFWriteAnyArray_i))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                    - (s IDTIFFWriteAnyArray_i)) (1)]
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                             - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFWriteAnyArray__tmp)
                                            - (s IDTIFFWriteAnyArray_i))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                    - (s IDTIFFWriteAnyArray_i)) (1)]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 66%positive => []
    | 67%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                             - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFWriteAnyArray__tmp)
                                            - (s IDTIFFWriteAnyArray_i))]
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                    - (s IDTIFFWriteAnyArray_i)) (1)]
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 82%positive => []
    | 83%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                             - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFWriteAnyArray__tmp)
                                            - (s IDTIFFWriteAnyArray_i))]
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                    - (s IDTIFFWriteAnyArray_i)) (1)]
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 98%positive => []
    | 99%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                             - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFWriteAnyArray__tmp)
                                            - (s IDTIFFWriteAnyArray_i))]
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => []
    | 104%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                     - (s IDTIFFWriteAnyArray_i)) (1)]
    | 105%positive => []
    | 106%positive => []
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => []
    | 113%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFWriteAnyArray_z))) (F_check_ge ((s IDTIFFWriteAnyArray_z)) (0))]
    | 114%positive => []
    | 115%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFWriteAnyArray__tmp)
                                                              - (s IDTIFFWriteAnyArray_i)) (-1
                                                                    + (s IDTIFFWriteAnyArray__tmp)
                                                                    - (s IDTIFFWriteAnyArray_i)));
                       (*-1 0*) F_max0_ge_0 (-1
                                             + (s IDTIFFWriteAnyArray__tmp)
                                             - (s IDTIFFWriteAnyArray_i))]
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => [(*0 1*) F_max0_pre_decrement ((s IDTIFFWriteAnyArray__tmp)
                                                     - (s IDTIFFWriteAnyArray_i)) (1)]
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | 130%positive => []
    | 131%positive => [(*-1 0*) F_max0_ge_0 ((s IDTIFFWriteAnyArray__tmp));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDTIFFWriteAnyArray_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDTIFFWriteAnyArray_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDTIFFWriteAnyArray_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDTIFFWriteAnyArray_z)))]
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => []
    | 137%positive => []
    | _ => []
  end.


Theorem TIFFWriteAnyArray_ai_correct:
  forall s p' s', steps (g_start TIFFWriteAnyArray) s (g_edges TIFFWriteAnyArray) p' s' -> TIFFWriteAnyArray_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFWriteAnyArray_pot_correct:
  forall s p' s',
    steps (g_start TIFFWriteAnyArray) s (g_edges TIFFWriteAnyArray) p' s' ->
    (TIFFWriteAnyArray_pot (g_start TIFFWriteAnyArray) s >= TIFFWriteAnyArray_pot p' s')%Q.
Proof.
  check_lp TIFFWriteAnyArray_ai_correct TIFFWriteAnyArray_hints.
Qed.

