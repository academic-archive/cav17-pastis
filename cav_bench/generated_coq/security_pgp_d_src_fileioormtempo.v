Require Import pasta.Pasta.

Notation IDrmtemp_z := 1%positive.
Notation IDrmtemp_i := 2%positive.
Notation IDrmtemp_verbose := 3%positive.
Notation IDrmtemp_name := 4%positive.
Definition rmtemp : graph := {|
  g_start := 1%positive;
  g_end := 53%positive;
  g_edges := (1%positive,(AAssign IDrmtemp_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDrmtemp_i (Some (ENum (0)))),3%positive)::
             (3%positive,ANone,4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_i) s) <
             (eval (ENum (8)) s))%Z)),7%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_i) s) >=
             (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,19%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (8%positive,ANone,11%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,17%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDrmtemp_i (Some (EAdd (EVar IDrmtemp_i)
             (ENum (1))))),13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDrmtemp_z (Some (EAdd (ENum (1))
             (EVar IDrmtemp_z)))),16%positive)::
             (16%positive,AWeaken,5%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_i) s) <
             (eval (ENum (8)) s))%Z)),21%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_i) s) >=
             (eval (ENum (8)) s))%Z)),20%positive)::
             (20%positive,AWeaken,53%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,24%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,27%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,32%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_verbose)
             s) <> (eval (ENum (0)) s))%Z)),29%positive)::
             (27%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_verbose)
             s) = (eval (ENum (0)) s))%Z)),28%positive)::
             (28%positive,AWeaken,31%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,51%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_verbose)
             s) <> (eval (ENum (0)) s))%Z)),35%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_verbose)
             s) = (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,38%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,40%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,42%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,44%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,50%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_verbose)
             s) <> (eval (ENum (0)) s))%Z)),47%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDrmtemp_verbose)
             s) = (eval (ENum (0)) s))%Z)),46%positive)::
             (46%positive,AWeaken,49%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,AWeaken,53%positive)::nil
|}.

Definition rmtemp_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_i) <= 0)%Z
    | 4%positive => (-1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 5%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -8 <= 0)%Z
    | 6%positive => (1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) + 8 <= 0)%Z
    | 7%positive => (-1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 8%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0)%Z
    | 9%positive => (-1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 10%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0)%Z
    | 11%positive => (-1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 12%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0)%Z
    | 13%positive => (-1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDrmtemp_i) + 1 <= 0 /\ 1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 15%positive => (-1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDrmtemp_i) + 1 <= 0 /\ 1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_z) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 18%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0)%Z
    | 19%positive => (1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 20%positive => (-1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_i) + 8 <= 0)%Z
    | 21%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 22%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 23%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 24%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 25%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 26%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 27%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 28%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0 /\ 1 * (s IDrmtemp_verbose) <= 0 /\ -1 * (s IDrmtemp_verbose) <= 0)%Z
    | 29%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 30%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 31%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 32%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 33%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 34%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0 /\ 1 * (s IDrmtemp_verbose) <= 0 /\ -1 * (s IDrmtemp_verbose) <= 0)%Z
    | 35%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 36%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 37%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 38%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 39%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 40%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 41%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 42%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 43%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 44%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 45%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 46%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0 /\ 1 * (s IDrmtemp_verbose) <= 0 /\ -1 * (s IDrmtemp_verbose) <= 0)%Z
    | 47%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 48%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 49%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 50%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 51%positive => (1 * (s IDrmtemp_i) + -7 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | 52%positive => (-1 * (s IDrmtemp_z) <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ 1 * (s IDrmtemp_i) + -7 <= 0)%Z
    | 53%positive => (1 * (s IDrmtemp_i) + -8 <= 0 /\ -1 * (s IDrmtemp_i) <= 0 /\ -1 * (s IDrmtemp_z) <= 0)%Z
    | _ => False
  end.

Definition rmtemp_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDrmtemp_z))%Q
    | 3%positive => ((s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 4%positive => ((s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 5%positive => ((s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 6%positive => ((s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 7%positive => ((s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 8%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 9%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 10%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 11%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 12%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 13%positive => ((1 # 1) + (s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 14%positive => ((1 # 1) + (s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 15%positive => ((1 # 1) + (s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 16%positive => ((s IDrmtemp_z) + max0(8 - (s IDrmtemp_i)))%Q
    | 17%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 18%positive => ((1 # 1) + (s IDrmtemp_z) + max0(7 - (s IDrmtemp_i)))%Q
    | 19%positive => ((s IDrmtemp_z))%Q
    | 20%positive => ((s IDrmtemp_z))%Q
    | 21%positive => ((s IDrmtemp_z))%Q
    | 22%positive => ((s IDrmtemp_z))%Q
    | 23%positive => ((s IDrmtemp_z))%Q
    | 24%positive => ((s IDrmtemp_z))%Q
    | 25%positive => ((s IDrmtemp_z))%Q
    | 26%positive => ((s IDrmtemp_z))%Q
    | 27%positive => ((s IDrmtemp_z))%Q
    | 28%positive => ((s IDrmtemp_z))%Q
    | 29%positive => ((s IDrmtemp_z))%Q
    | 30%positive => ((s IDrmtemp_z))%Q
    | 31%positive => ((s IDrmtemp_z))%Q
    | 32%positive => ((s IDrmtemp_z))%Q
    | 33%positive => ((s IDrmtemp_z))%Q
    | 34%positive => ((s IDrmtemp_z))%Q
    | 35%positive => ((s IDrmtemp_z))%Q
    | 36%positive => ((s IDrmtemp_z))%Q
    | 37%positive => ((s IDrmtemp_z))%Q
    | 38%positive => ((s IDrmtemp_z))%Q
    | 39%positive => ((s IDrmtemp_z))%Q
    | 40%positive => ((s IDrmtemp_z))%Q
    | 41%positive => ((s IDrmtemp_z))%Q
    | 42%positive => ((s IDrmtemp_z))%Q
    | 43%positive => ((s IDrmtemp_z))%Q
    | 44%positive => ((s IDrmtemp_z))%Q
    | 45%positive => ((s IDrmtemp_z))%Q
    | 46%positive => ((s IDrmtemp_z))%Q
    | 47%positive => ((s IDrmtemp_z))%Q
    | 48%positive => ((s IDrmtemp_z))%Q
    | 49%positive => ((s IDrmtemp_z))%Q
    | 50%positive => ((s IDrmtemp_z))%Q
    | 51%positive => ((s IDrmtemp_z))%Q
    | 52%positive => ((s IDrmtemp_z))%Q
    | 53%positive => ((s IDrmtemp_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition rmtemp_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDrmtemp_i)) (7
                                                                    - (s IDrmtemp_i)));
                     (*-1 0*) F_max0_ge_0 (7 - (s IDrmtemp_i))]
    | 7%positive => [(*-1.40406e-12 1*) F_max0_pre_decrement (8
                                                              - (s IDrmtemp_i)) (1)]
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
    | 18%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (7 - (s IDrmtemp_i))]
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
    | _ => []
  end.


Theorem rmtemp_ai_correct:
  forall s p' s', steps (g_start rmtemp) s (g_edges rmtemp) p' s' -> rmtemp_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem rmtemp_pot_correct:
  forall s p' s',
    steps (g_start rmtemp) s (g_edges rmtemp) p' s' ->
    (rmtemp_pot (g_start rmtemp) s >= rmtemp_pot p' s')%Q.
Proof.
  check_lp rmtemp_ai_correct rmtemp_hints.
Qed.

