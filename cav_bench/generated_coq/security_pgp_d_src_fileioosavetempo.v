Require Import pasta.Pasta.

Notation IDsavetemp_z := 1%positive.
Notation IDsavetemp_i := 2%positive.
Notation IDsavetemp_verbose := 3%positive.
Notation IDsavetemp_name := 4%positive.
Notation IDsavetemp_newname := 5%positive.
Definition savetemp : graph := {|
  g_start := 1%positive;
  g_end := 58%positive;
  g_edges := (1%positive,(AAssign IDsavetemp_z (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::
             (3%positive,ANone,56%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDsavetemp_i (Some (ENum (0)))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_i) s) <
             (eval (ENum (8)) s))%Z)),9%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_i) s) >=
             (eval (ENum (8)) s))%Z)),8%positive)::
             (8%positive,AWeaken,21%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,13%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,19%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDsavetemp_i
             (Some (EAdd (EVar IDsavetemp_i) (ENum (1))))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDsavetemp_z (Some (EAdd (ENum (1))
             (EVar IDsavetemp_z)))),18%positive)::
             (18%positive,AWeaken,7%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_i) s) <
             (eval (ENum (8)) s))%Z)),23%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_i) s) >=
             (eval (ENum (8)) s))%Z)),22%positive)::
             (22%positive,AWeaken,29%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,49%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,48%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,46%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_verbose)
             s) <> (eval (ENum (0)) s))%Z)),33%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_verbose)
             s) = (eval (ENum (0)) s))%Z)),32%positive)::
             (32%positive,AWeaken,36%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,44%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_i) s) <
             (eval (ENum (8)) s))%Z)),40%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_i) s) >=
             (eval (ENum (8)) s))%Z)),39%positive)::
             (39%positive,AWeaken,42%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,58%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,58%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,58%positive)::
             (48%positive,AWeaken,50%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_verbose)
             s) <> (eval (ENum (0)) s))%Z)),52%positive)::
             (50%positive,(AGuard (fun s => ((eval (EVar IDsavetemp_verbose)
             s) = (eval (ENum (0)) s))%Z)),51%positive)::
             (51%positive,AWeaken,54%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,58%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::nil
|}.

Definition savetemp_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_z) <= 0)%Z
    | 4%positive => (1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 6%positive => (-1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 7%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 8%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) + 8 <= 0)%Z
    | 9%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 10%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 11%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 12%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 13%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 14%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 15%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDsavetemp_i) + 1 <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 17%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDsavetemp_i) + 1 <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 20%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 21%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 22%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_i) + 8 <= 0)%Z
    | 23%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 24%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 25%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 26%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 27%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 28%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 29%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 30%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 31%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 32%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ 1 * (s IDsavetemp_verbose) <= 0 /\ -1 * (s IDsavetemp_verbose) <= 0)%Z
    | 33%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 34%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 35%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 36%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 37%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 38%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 39%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_i) + 8 <= 0)%Z
    | 40%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 41%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 42%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 43%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 44%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 45%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 46%positive => (-1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_i) + -8 <= 0)%Z
    | 47%positive => (1 * (s IDsavetemp_i) + -8 <= 0 /\ -1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0)%Z
    | 48%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 49%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 50%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 51%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0 /\ 1 * (s IDsavetemp_verbose) <= 0 /\ -1 * (s IDsavetemp_verbose) <= 0)%Z
    | 52%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 53%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 54%positive => (-1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ 1 * (s IDsavetemp_i) + -7 <= 0)%Z
    | 55%positive => (1 * (s IDsavetemp_i) + -7 <= 0 /\ -1 * (s IDsavetemp_i) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 56%positive => (1 * (s IDsavetemp_z) <= 0 /\ -1 * (s IDsavetemp_z) <= 0)%Z
    | 57%positive => (-1 * (s IDsavetemp_z) <= 0 /\ 1 * (s IDsavetemp_z) <= 0)%Z
    | 58%positive => (-1 * (s IDsavetemp_z) <= 0)%Z
    | _ => False
  end.

Definition savetemp_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDsavetemp_z))%Q
    | 3%positive => ((8 # 1) + (s IDsavetemp_z))%Q
    | 4%positive => ((8 # 1) + (s IDsavetemp_z))%Q
    | 5%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 6%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 7%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 8%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 9%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 10%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 11%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 12%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 13%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 14%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 15%positive => ((9 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 16%positive => ((9 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 17%positive => ((9 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 18%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 19%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 20%positive => ((8 # 1) - (s IDsavetemp_i) + (s IDsavetemp_z))%Q
    | 21%positive => ((s IDsavetemp_z))%Q
    | 22%positive => ((s IDsavetemp_z))%Q
    | 23%positive => ((s IDsavetemp_z))%Q
    | 24%positive => ((s IDsavetemp_z))%Q
    | 25%positive => ((s IDsavetemp_z))%Q
    | 26%positive => ((s IDsavetemp_z))%Q
    | 27%positive => ((s IDsavetemp_z))%Q
    | 28%positive => ((s IDsavetemp_z))%Q
    | 29%positive => ((s IDsavetemp_z))%Q
    | 30%positive => ((s IDsavetemp_z))%Q
    | 31%positive => ((s IDsavetemp_z))%Q
    | 32%positive => ((s IDsavetemp_z))%Q
    | 33%positive => ((s IDsavetemp_z))%Q
    | 34%positive => ((s IDsavetemp_z))%Q
    | 35%positive => ((s IDsavetemp_z))%Q
    | 36%positive => ((s IDsavetemp_z))%Q
    | 37%positive => ((s IDsavetemp_z))%Q
    | 38%positive => ((s IDsavetemp_z))%Q
    | 39%positive => ((s IDsavetemp_z))%Q
    | 40%positive => ((s IDsavetemp_z))%Q
    | 41%positive => ((s IDsavetemp_z))%Q
    | 42%positive => ((s IDsavetemp_z))%Q
    | 43%positive => ((s IDsavetemp_z))%Q
    | 44%positive => ((s IDsavetemp_z))%Q
    | 45%positive => ((s IDsavetemp_z))%Q
    | 46%positive => ((s IDsavetemp_z))%Q
    | 47%positive => ((s IDsavetemp_z))%Q
    | 48%positive => ((s IDsavetemp_z))%Q
    | 49%positive => ((s IDsavetemp_z))%Q
    | 50%positive => ((s IDsavetemp_z))%Q
    | 51%positive => ((s IDsavetemp_z))%Q
    | 52%positive => ((s IDsavetemp_z))%Q
    | 53%positive => ((s IDsavetemp_z))%Q
    | 54%positive => ((s IDsavetemp_z))%Q
    | 55%positive => ((s IDsavetemp_z))%Q
    | 56%positive => ((8 # 1) + (s IDsavetemp_z))%Q
    | 57%positive => ((8 # 1) + (s IDsavetemp_z))%Q
    | 58%positive => ((s IDsavetemp_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition savetemp_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDsavetemp_i)) (7
                                                                    - (s IDsavetemp_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDsavetemp_i));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDsavetemp_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDsavetemp_i)))]
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
    | 20%positive => [(*-1.14286 0*) F_max0_ge_0 (7 - (s IDsavetemp_i));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDsavetemp_i))) (F_check_ge (0) (0));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsavetemp_i)) (0))) (F_max0_ge_0 ((s IDsavetemp_i)));
                      (*-1.14286 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDsavetemp_i)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDsavetemp_i)))]
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
    | 56%positive => []
    | 57%positive => [(*-8 0*) F_one]
    | 58%positive => []
    | _ => []
  end.


Theorem savetemp_ai_correct:
  forall s p' s', steps (g_start savetemp) s (g_edges savetemp) p' s' -> savetemp_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem savetemp_pot_correct:
  forall s p' s',
    steps (g_start savetemp) s (g_edges savetemp) p' s' ->
    (savetemp_pot (g_start savetemp) s >= savetemp_pot p' s')%Q.
Proof.
  check_lp savetemp_ai_correct savetemp_hints.
Qed.

