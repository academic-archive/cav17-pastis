Require Import pasta.Pasta.

Notation IDcmd_put_ht_order_z := 1%positive.
Notation IDcmd_put_ht_order__tmp := 2%positive.
Notation IDcmd_put_ht_order__tmp1 := 3%positive.
Notation IDcmd_put_ht_order__tmp2 := 4%positive.
Notation IDcmd_put_ht_order_i := 5%positive.
Notation IDcmd_put_ht_order_len := 6%positive.
Notation IDcmd_put_ht_order_n := 7%positive.
Notation IDcmd_put_ht_order_porder_dref_off56 := 8%positive.
Notation IDcmd_put_ht_order_porder_dref_off60 := 9%positive.
Notation IDcmd_put_ht_order_cldev := 10%positive.
Notation IDcmd_put_ht_order_cname := 11%positive.
Notation IDcmd_put_ht_order_component := 12%positive.
Notation IDcmd_put_ht_order_porder := 13%positive.
Definition cmd_put_ht_order : graph := {|
  g_start := 1%positive;
  g_end := 77%positive;
  g_edges := (1%positive,(AAssign IDcmd_put_ht_order_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_porder_dref_off60)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_porder_dref_off56)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDcmd_put_ht_order_n)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDcmd_put_ht_order_i)
             s) >= (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDcmd_put_ht_order__tmp2
             (Some (EVar IDcmd_put_ht_order_cname))),8%positive)::
             (8%positive,(AAssign IDcmd_put_ht_order__tmp
             (Some (EVar IDcmd_put_ht_order_component))),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order__tmp) s) >=
             (eval (ENum (0)) s))%Z)),12%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order__tmp) s) <
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,14%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDcmd_put_ht_order_len None),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,74%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcmd_put_ht_order_i (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_i) s) <
             (eval (EVar IDcmd_put_ht_order_porder_dref_off56) s))%Z)),
             53%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_i) s) >=
             (eval (EVar IDcmd_put_ht_order_porder_dref_off56) s))%Z)),
             23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDcmd_put_ht_order_i (Some (ENum (0)))),
             25%positive)::(25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_i) s) <
             (eval (EVar IDcmd_put_ht_order_porder_dref_off60) s))%Z)),
             32%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_i) s) >=
             (eval (EVar IDcmd_put_ht_order_porder_dref_off60) s))%Z)),
             28%positive)::(28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDcmd_put_ht_order__tmp1
             (Some (ENum (0)))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,77%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDcmd_put_ht_order_n
             (Some (ESub (EVar IDcmd_put_ht_order_porder_dref_off60)
             (EVar IDcmd_put_ht_order_i)))),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_n) s) >
             (eval (ENum (99)) s))%Z)),37%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_n) s) <=
             (eval (ENum (99)) s))%Z)),36%positive)::
             (36%positive,AWeaken,40%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDcmd_put_ht_order_n (Some (ENum (99)))),
             39%positive)::(39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,50%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDcmd_put_ht_order_i
             (Some (EAdd (EVar IDcmd_put_ht_order_i)
             (EVar IDcmd_put_ht_order_n)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDcmd_put_ht_order_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_put_ht_order_z)))),
             49%positive)::(49%positive,AWeaken,27%positive)::
             (50%positive,(AAssign IDcmd_put_ht_order__tmp1 None),
             51%positive)::(51%positive,ANone,52%positive)::
             (52%positive,AWeaken,77%positive)::
             (53%positive,AWeaken,54%positive)::
             (54%positive,(AAssign IDcmd_put_ht_order_n
             (Some (ESub (EVar IDcmd_put_ht_order_porder_dref_off56)
             (EVar IDcmd_put_ht_order_i)))),55%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_n) s) >
             (eval (ENum (199)) s))%Z)),58%positive)::
             (56%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_put_ht_order_n) s) <=
             (eval (ENum (199)) s))%Z)),57%positive)::
             (57%positive,AWeaken,61%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AAssign IDcmd_put_ht_order_n (Some (ENum (199)))),
             60%positive)::(60%positive,ANone,61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,71%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDcmd_put_ht_order_i
             (Some (EAdd (EVar IDcmd_put_ht_order_i)
             (EVar IDcmd_put_ht_order_n)))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,(AAssign IDcmd_put_ht_order_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_put_ht_order_z)))),
             70%positive)::(70%positive,AWeaken,22%positive)::
             (71%positive,(AAssign IDcmd_put_ht_order__tmp1 None),
             72%positive)::(72%positive,ANone,73%positive)::
             (73%positive,AWeaken,77%positive)::
             (74%positive,(AAssign IDcmd_put_ht_order__tmp1 None),
             75%positive)::(75%positive,ANone,76%positive)::
             (76%positive,AWeaken,77%positive)::nil
|}.

Definition cmd_put_ht_order_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 4%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 6%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 7%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 8%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 9%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 10%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 11%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ 1 * (s IDcmd_put_ht_order__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order__tmp) <= 0)%Z
    | 13%positive => (-1 * (s IDcmd_put_ht_order__tmp) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 17%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 18%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 19%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 20%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ 1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 21%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ 1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 22%positive => (-1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 23%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_i)+ 1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 24%positive => (-1 * (s IDcmd_put_ht_order_i)+ 1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 25%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 26%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ 1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 27%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 28%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_i)+ 1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 29%positive => (-1 * (s IDcmd_put_ht_order_i)+ 1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 30%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_i)+ 1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order__tmp1) <= 0 /\ -1 * (s IDcmd_put_ht_order__tmp1) <= 0)%Z
    | 31%positive => (-1 * (s IDcmd_put_ht_order__tmp1) <= 0 /\ 1 * (s IDcmd_put_ht_order__tmp1) <= 0 /\ -1 * (s IDcmd_put_ht_order_i)+ 1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 32%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 34%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 36%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0)%Z
    | 37%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 100 <= 0)%Z
    | 38%positive => (-1 * (s IDcmd_put_ht_order_n) + 100 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 39%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 99 <= 0)%Z
    | 40%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 41%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 43%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 45%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | 52%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off60) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -99 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0)%Z
    | 54%positive => (1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 55%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 57%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0)%Z
    | 58%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 200 <= 0)%Z
    | 59%positive => (-1 * (s IDcmd_put_ht_order_n) + 200 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n) + 2 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 60%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 199 <= 0)%Z
    | 61%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 62%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 64%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 66%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0)%Z
    | 68%positive => (1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 69%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0)%Z
    | 70%positive => (1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_n)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 72%positive => (-1 * (s IDcmd_put_ht_order_n) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0)%Z
    | 73%positive => (-1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_i)+ -1 * (s IDcmd_put_ht_order_porder_dref_off56) + 1 <= 0 /\ 1 * (s IDcmd_put_ht_order_n) + -199 <= 0 /\ -1 * (s IDcmd_put_ht_order_n) + 1 <= 0)%Z
    | 74%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 75%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_i) <= 0)%Z
    | 76%positive => (-1 * (s IDcmd_put_ht_order_i) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ 1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_n) <= 0)%Z
    | 77%positive => (-1 * (s IDcmd_put_ht_order_n) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off60) <= 0 /\ -1 * (s IDcmd_put_ht_order_z) <= 0 /\ -1 * (s IDcmd_put_ht_order_porder_dref_off56) <= 0)%Z
    | _ => False
  end.

Definition cmd_put_ht_order_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 2%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 3%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 4%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 5%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 6%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 7%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 8%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 9%positive => ((s IDcmd_put_ht_order_z)
                     + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                     + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 10%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 11%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 12%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 13%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 14%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 15%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 16%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 17%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 18%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 19%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 20%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 21%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 22%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 23%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 24%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 25%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 26%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 27%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 28%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 29%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 30%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 31%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 32%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 33%positive => (-(32 # 127) - (65 # 129) * (s IDcmd_put_ht_order_i)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 34%positive => (-(32 # 127) - (65 # 129) * (s IDcmd_put_ht_order_i)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 35%positive => (-(32 # 127) - (65 # 129) * (s IDcmd_put_ht_order_i)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 36%positive => (-(32 # 127) - (65 # 129) * (s IDcmd_put_ht_order_i)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 37%positive => (-(32 # 127) - (65 # 129) * (s IDcmd_put_ht_order_i)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 38%positive => (-(32 # 127) - (65 # 129) * (s IDcmd_put_ht_order_i)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 39%positive => (-(65 # 129) * (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60))
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 40%positive => (-(65 # 129) * (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60))
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 41%positive => (-(65 # 129) * (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (65 # 129) * (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      + (64 # 129) * max0(198 - (s IDcmd_put_ht_order_i)
                                          + (s IDcmd_put_ht_order_porder_dref_off60))
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 42%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 43%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 44%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 45%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 46%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      + (1 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 47%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      + (1 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 48%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      + (1 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 49%positive => ((12351 # 127) - (s IDcmd_put_ht_order_i)
                      + (1 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 50%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 51%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 52%positive => ((12478 # 127) - (s IDcmd_put_ht_order_i)
                      - (0 # 1) * (s IDcmd_put_ht_order_n)
                      + (s IDcmd_put_ht_order_porder_dref_off60)
                      + (s IDcmd_put_ht_order_z)
                      - (0 # 1) * max0(199 - (s IDcmd_put_ht_order_n)))%Q
    | 53%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 54%positive => ((198 # 1) - (s IDcmd_put_ht_order_i)
                      + (s IDcmd_put_ht_order_porder_dref_off56)
                      + (s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 55%positive => ((198 # 1) - (s IDcmd_put_ht_order_i)
                      + (s IDcmd_put_ht_order_porder_dref_off56)
                      + (s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 56%positive => ((198 # 1) - (s IDcmd_put_ht_order_i)
                      + (s IDcmd_put_ht_order_porder_dref_off56)
                      + (s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 57%positive => ((198 # 1) - (s IDcmd_put_ht_order_i)
                      + (s IDcmd_put_ht_order_porder_dref_off56)
                      + (s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 58%positive => ((198 # 1) - (s IDcmd_put_ht_order_i)
                      + (s IDcmd_put_ht_order_porder_dref_off56)
                      + (s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60)))%Q
    | 59%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 60%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 61%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 62%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 63%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 64%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 65%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 66%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 67%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 68%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 69%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 70%positive => (-(1 # 1) + (s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 71%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 72%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 73%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 - (s IDcmd_put_ht_order_i)
                             - (s IDcmd_put_ht_order_n)
                             + (s IDcmd_put_ht_order_porder_dref_off56))
                      + max0((s IDcmd_put_ht_order_n)))%Q
    | 74%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 75%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 76%positive => ((s IDcmd_put_ht_order_z)
                      + max0(98 + (s IDcmd_put_ht_order_porder_dref_off60))
                      + max0(198 + (s IDcmd_put_ht_order_porder_dref_off56)))%Q
    | 77%positive => ((s IDcmd_put_ht_order_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_put_ht_order_hints (p : node) (s : state) := 
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
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (198
                                                             - (s IDcmd_put_ht_order_i)
                                                             + (s IDcmd_put_ht_order_porder_dref_off56)) (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    - (s IDcmd_put_ht_order_n)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off56)));
                      (*-1 0*) F_max0_ge_0 (198 - (s IDcmd_put_ht_order_i)
                                            - (s IDcmd_put_ht_order_n)
                                            + (s IDcmd_put_ht_order_porder_dref_off56))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (98
                                                             - (s IDcmd_put_ht_order_i)
                                                             + (s IDcmd_put_ht_order_porder_dref_off60)) (98
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    - (s IDcmd_put_ht_order_n)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)));
                      (*-1 0*) F_max0_ge_0 (98 - (s IDcmd_put_ht_order_i)
                                            - (s IDcmd_put_ht_order_n)
                                            + (s IDcmd_put_ht_order_porder_dref_off60))]
    | 32%positive => [(*0 0.496222*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)) (0))) (F_max0_ge_0 (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (98
                                                                  - (s IDcmd_put_ht_order_i)
                                                                  + (s IDcmd_put_ht_order_porder_dref_off60))) (F_check_ge (98
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)) (0))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*0 0.00126577*) F_binom_monotonic 1 (F_max0_ge_arg (199
                                                                    - (s IDcmd_put_ht_order_n))) (F_check_ge (199
                                                                    - (s IDcmd_put_ht_order_n)) (0))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-0.496222 0*) F_binom_monotonic 1 (F_max0_ge_arg (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60))) (F_check_ge (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)) (0))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*-0.00126577 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (199
                                                                    - (s IDcmd_put_ht_order_n)) (0))) (F_max0_ge_0 (199
                                                                    - (s IDcmd_put_ht_order_n)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (98
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)) (0))) (F_max0_ge_0 (98
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off60)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDcmd_put_ht_order_n))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcmd_put_ht_order_n)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcmd_put_ht_order_n)))]
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*-98.5 0*) F_one;
                      (*-0.5 0*) F_max0_monotonic (F_check_ge (-1
                                                               - 2 * (s IDcmd_put_ht_order_i)
                                                               + 2 * (s IDcmd_put_ht_order_porder_dref_off60)) (-1
                                                                    - 2 * (s IDcmd_put_ht_order_i)
                                                                    - 2 * (s IDcmd_put_ht_order_n)
                                                                    + 2 * (s IDcmd_put_ht_order_porder_dref_off60)));
                      (*0 0.5*) F_max0_ge_0 (-1
                                             - 2 * (s IDcmd_put_ht_order_i)
                                             - 2 * (s IDcmd_put_ht_order_n)
                                             + 2 * (s IDcmd_put_ht_order_porder_dref_off60));
                      (*-0.00126577 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (199
                                                                    - (s IDcmd_put_ht_order_n)) (0))) (F_max0_ge_0 (199
                                                                    - (s IDcmd_put_ht_order_n)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - 2 * (s IDcmd_put_ht_order_i)
                                                                    + 2 * (s IDcmd_put_ht_order_porder_dref_off60)) (0))) (F_max0_ge_0 (-1
                                                                    - 2 * (s IDcmd_put_ht_order_i)
                                                                    + 2 * (s IDcmd_put_ht_order_porder_dref_off60)))]
    | 53%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (198
                                                                  - (s IDcmd_put_ht_order_i)
                                                                  + (s IDcmd_put_ht_order_porder_dref_off56))) (F_check_ge (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off56)) (0))]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off56)) (0))) (F_max0_ge_0 (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off56)))]
    | 58%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off56)) (0))) (F_max0_ge_0 (198
                                                                    - (s IDcmd_put_ht_order_i)
                                                                    + (s IDcmd_put_ht_order_porder_dref_off56)))]
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*-1 0*) F_max0_pre_decrement (198
                                                     - (s IDcmd_put_ht_order_i)
                                                     + (s IDcmd_put_ht_order_porder_dref_off56)) ((s IDcmd_put_ht_order_n));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_put_ht_order_n)) (0))) (F_max0_ge_0 ((s IDcmd_put_ht_order_n)))]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_put_ht_order_n))) (F_check_ge ((s IDcmd_put_ht_order_n)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDcmd_put_ht_order_n))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDcmd_put_ht_order_n)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDcmd_put_ht_order_n)))]
    | 71%positive => []
    | 72%positive => []
    | 73%positive => [(*-1 0*) F_max0_ge_0 (98
                                            + (s IDcmd_put_ht_order_porder_dref_off60));
                      (*-1 0*) F_max0_ge_0 (198 - (s IDcmd_put_ht_order_i)
                                            - (s IDcmd_put_ht_order_n)
                                            + (s IDcmd_put_ht_order_porder_dref_off56));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcmd_put_ht_order_n))) (F_check_ge (0) (0))]
    | 74%positive => []
    | 75%positive => []
    | 76%positive => [(*-1 0*) F_max0_ge_0 (98
                                            + (s IDcmd_put_ht_order_porder_dref_off60));
                      (*-1 0*) F_max0_ge_0 (198
                                            + (s IDcmd_put_ht_order_porder_dref_off56))]
    | 77%positive => []
    | _ => []
  end.


Theorem cmd_put_ht_order_ai_correct:
  forall s p' s', steps (g_start cmd_put_ht_order) s (g_edges cmd_put_ht_order) p' s' -> cmd_put_ht_order_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_put_ht_order_pot_correct:
  forall s p' s',
    steps (g_start cmd_put_ht_order) s (g_edges cmd_put_ht_order) p' s' ->
    (cmd_put_ht_order_pot (g_start cmd_put_ht_order) s >= cmd_put_ht_order_pot p' s')%Q.
Proof.
  check_lp cmd_put_ht_order_ai_correct cmd_put_ht_order_hints.
Qed.

