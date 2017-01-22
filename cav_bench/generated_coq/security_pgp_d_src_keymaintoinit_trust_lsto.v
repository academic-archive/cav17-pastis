Require Import pasta.Pasta.

Notation IDinit_trust_lst_z := 1%positive.
Notation IDinit_trust_lst_i := 2%positive.
Notation IDinit_trust_lst_init_trust_lst.initialized := 3%positive.
Notation IDinit_trust_lst_legitlst_len := 4%positive.
Notation IDinit_trust_lst_len := 5%positive.
Notation IDinit_trust_lst_trustlst_len := 6%positive.
Definition init_trust_lst : graph := {|
  g_start := 1%positive;
  g_end := 56%positive;
  g_edges := (1%positive,(AAssign IDinit_trust_lst_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDinit_trust_lst_init_trust_lst.initialized)
             s) <> (eval (ENum (0)) s))%Z)),53%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDinit_trust_lst_init_trust_lst.initialized)
             s) = (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDinit_trust_lst_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDinit_trust_lst_i)
             s) < (eval (ENum (8)) s))%Z)),34%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDinit_trust_lst_i)
             s) >= (eval (ENum (8)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDinit_trust_lst_i (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDinit_trust_lst_i)
             s) < (eval (ENum (4)) s))%Z)),18%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDinit_trust_lst_i)
             s) >= (eval (ENum (4)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign
             IDinit_trust_lst_init_trust_lst.initialized (Some (ENum (1)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,56%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (19%positive,ANone,21%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDinit_trust_lst_len None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDinit_trust_lst_len) s) >
             (eval (EVar IDinit_trust_lst_legitlst_len) s))%Z)),25%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDinit_trust_lst_len) s) <=
             (eval (EVar IDinit_trust_lst_legitlst_len) s))%Z)),24%positive)::
             (24%positive,AWeaken,28%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDinit_trust_lst_legitlst_len
             (Some (EVar IDinit_trust_lst_len))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDinit_trust_lst_i
             (Some (EAdd (EVar IDinit_trust_lst_i) (ENum (1))))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDinit_trust_lst_z (Some (EAdd (ENum (1))
             (EVar IDinit_trust_lst_z)))),33%positive)::
             (33%positive,AWeaken,13%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (35%positive,ANone,47%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (37%positive,ANone,39%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDinit_trust_lst_len None),40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AGuard
             (fun s => ((eval (EVar IDinit_trust_lst_len) s) >
             (eval (EVar IDinit_trust_lst_trustlst_len) s))%Z)),43%positive)::
             (41%positive,(AGuard
             (fun s => ((eval (EVar IDinit_trust_lst_len) s) <=
             (eval (EVar IDinit_trust_lst_trustlst_len) s))%Z)),42%positive)::
             (42%positive,AWeaken,46%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AAssign IDinit_trust_lst_trustlst_len
             (Some (EVar IDinit_trust_lst_len))),45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDinit_trust_lst_i
             (Some (EAdd (EVar IDinit_trust_lst_i) (ENum (1))))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDinit_trust_lst_z (Some (EAdd (ENum (1))
             (EVar IDinit_trust_lst_z)))),52%positive)::
             (52%positive,AWeaken,8%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,56%positive)::nil
|}.

Definition init_trust_lst_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 4%positive => (1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 5%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 6%positive => (1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0)%Z
    | 7%positive => (-1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 8%positive => (-1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -8 <= 0)%Z
    | 9%positive => (1 * (s IDinit_trust_lst_i) + -8 <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) + 8 <= 0)%Z
    | 10%positive => (-1 * (s IDinit_trust_lst_i) + 8 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -8 <= 0)%Z
    | 11%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0)%Z
    | 12%positive => (-1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 13%positive => (-1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -4 <= 0)%Z
    | 14%positive => (1 * (s IDinit_trust_lst_i) + -4 <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) + 4 <= 0)%Z
    | 15%positive => (-1 * (s IDinit_trust_lst_i) + 4 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDinit_trust_lst_i) + -4 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) + 4 <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) + -1 <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) + -1 <= 0 /\ -1 * (s IDinit_trust_lst_i) + 4 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -4 <= 0)%Z
    | 18%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 20%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 22%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0)%Z
    | 23%positive => (1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 24%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ -1 * (s IDinit_trust_lst_legitlst_len)+ 1 * (s IDinit_trust_lst_len) <= 0)%Z
    | 25%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ 1 * (s IDinit_trust_lst_legitlst_len)+ -1 * (s IDinit_trust_lst_len) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDinit_trust_lst_legitlst_len)+ -1 * (s IDinit_trust_lst_len) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 27%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0)%Z
    | 28%positive => (1 * (s IDinit_trust_lst_i) + -3 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 29%positive => (-1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -3 <= 0)%Z
    | 30%positive => (-1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_i) + -4 <= 0)%Z
    | 31%positive => (1 * (s IDinit_trust_lst_i) + -4 <= 0 /\ -1 * (s IDinit_trust_lst_i) + 1 <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 32%positive => (-1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_i) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDinit_trust_lst_i) + -4 <= 0 /\ -1 * (s IDinit_trust_lst_i) + 1 <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0)%Z
    | 35%positive => (1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 36%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0)%Z
    | 37%positive => (1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 38%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0)%Z
    | 39%positive => (1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 40%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0)%Z
    | 41%positive => (1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 42%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ 1 * (s IDinit_trust_lst_len)+ -1 * (s IDinit_trust_lst_trustlst_len) <= 0)%Z
    | 43%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_len)+ 1 * (s IDinit_trust_lst_trustlst_len) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDinit_trust_lst_len)+ 1 * (s IDinit_trust_lst_trustlst_len) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 45%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0)%Z
    | 46%positive => (1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 47%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -7 <= 0)%Z
    | 48%positive => (1 * (s IDinit_trust_lst_i) + -7 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_i) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 49%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -8 <= 0 /\ -1 * (s IDinit_trust_lst_i) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDinit_trust_lst_i) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_i) + -8 <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0)%Z
    | 51%positive => (1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_i) + -8 <= 0 /\ -1 * (s IDinit_trust_lst_i) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDinit_trust_lst_i) + 1 <= 0 /\ 1 * (s IDinit_trust_lst_i) + -8 <= 0 /\ -1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ 1 * (s IDinit_trust_lst_init_trust_lst.initialized) <= 0 /\ -1 * (s IDinit_trust_lst_z) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 54%positive => (-1 * (s IDinit_trust_lst_z) <= 0 /\ 1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 55%positive => (1 * (s IDinit_trust_lst_z) <= 0 /\ -1 * (s IDinit_trust_lst_z) <= 0)%Z
    | 56%positive => (-1 * (s IDinit_trust_lst_z) <= 0)%Z
    | _ => False
  end.

Definition init_trust_lst_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((12 # 1))%Q
    | 2%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 3%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 4%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 5%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 6%positive => ((4 # 1) + (s IDinit_trust_lst_z)
                     + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 7%positive => ((4 # 1) + (s IDinit_trust_lst_z)
                     + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 8%positive => ((4 # 1) + (s IDinit_trust_lst_z)
                     + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 9%positive => ((4 # 1) + (s IDinit_trust_lst_z)
                     + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 10%positive => ((4 # 1) + (s IDinit_trust_lst_z))%Q
    | 11%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 12%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 13%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 14%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 15%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 16%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 17%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 18%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 19%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 20%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 21%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 22%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 23%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 24%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 25%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 26%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(3 - (s IDinit_trust_lst_i)))%Q
    | 27%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(3 - (s IDinit_trust_lst_i)))%Q
    | 28%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(3 - (s IDinit_trust_lst_i)))%Q
    | 29%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(3 - (s IDinit_trust_lst_i)))%Q
    | 30%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 31%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 32%positive => ((1 # 1) + (s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 33%positive => ((s IDinit_trust_lst_z)
                      + max0(4 - (s IDinit_trust_lst_i)))%Q
    | 34%positive => ((4 # 1) + (s IDinit_trust_lst_z)
                      + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 35%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 36%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 37%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 38%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 39%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 40%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 41%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 42%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 43%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 44%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 45%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 46%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 47%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 48%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(7 - (s IDinit_trust_lst_i)))%Q
    | 49%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 50%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 51%positive => ((5 # 1) + (s IDinit_trust_lst_z)
                      + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 52%positive => ((4 # 1) + (s IDinit_trust_lst_z)
                      + max0(8 - (s IDinit_trust_lst_i)))%Q
    | 53%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 54%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 55%positive => ((12 # 1) + (s IDinit_trust_lst_z))%Q
    | 56%positive => ((s IDinit_trust_lst_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition init_trust_lst_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDinit_trust_lst_i)) (7
                                                                    - (s IDinit_trust_lst_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDinit_trust_lst_i))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDinit_trust_lst_i)) (3
                                                                    - (s IDinit_trust_lst_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDinit_trust_lst_i))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*0 1*) F_max0_pre_decrement (4
                                                    - (s IDinit_trust_lst_i)) (1)]
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDinit_trust_lst_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*0 1*) F_max0_pre_decrement (8
                                                    - (s IDinit_trust_lst_i)) (1)]
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
    | 55%positive => [(*-12 0*) F_one]
    | 56%positive => []
    | _ => []
  end.


Theorem init_trust_lst_ai_correct:
  forall s p' s', steps (g_start init_trust_lst) s (g_edges init_trust_lst) p' s' -> init_trust_lst_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem init_trust_lst_pot_correct:
  forall s p' s',
    steps (g_start init_trust_lst) s (g_edges init_trust_lst) p' s' ->
    (init_trust_lst_pot (g_start init_trust_lst) s >= init_trust_lst_pot p' s')%Q.
Proof.
  check_lp init_trust_lst_ai_correct init_trust_lst_hints.
Qed.

