Require Import pasta.Pasta.

Notation IDsputs_z := 1%positive.
Notation IDsputs__tmp := 2%positive.
Notation IDsputs_ch := 3%positive.
Notation IDsputs_count := 4%positive.
Notation IDsputs_len := 5%positive.
Notation IDsputs_pn_dref := 6%positive.
Notation IDsputs_status := 7%positive.
Notation IDsputs_pn := 8%positive.
Notation IDsputs_s := 9%positive.
Notation IDsputs_str := 10%positive.
Notation IDsputs_wlen := 11%positive.
Definition sputs : graph := {|
  g_start := 1%positive;
  g_end := 44%positive;
  g_edges := (1%positive,(AAssign IDsputs_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDsputs_len) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDsputs_count) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDsputs__tmp (Some (EVar IDsputs_wlen))),
             6%positive)::
             (6%positive,(AAssign IDsputs_len (Some (EVar IDsputs__tmp))),
             7%positive)::
             (7%positive,(AAssign IDsputs_status None),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDsputs_status) s) >=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDsputs_status) s) <
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,35%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDsputs_len) s) >
             (eval (ENum (0)) s))%Z)),16%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDsputs_len) s) <=
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,34%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDsputs_count None),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDsputs_count) s) >
             (eval (ENum (0)) s))%Z)),45%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDsputs_count) s) <=
             (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDsputs_ch None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,25%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,30%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDsputs_status None),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard (fun s => True)),32%positive)::
             (28%positive,(AGuard (fun s => True)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDsputs_len (Some (EAdd (EVar IDsputs_len)
             (ENum (-1))))),31%positive)::(31%positive,ANone,53%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDsputs_pn_dref
             (Some (ESub (EVar IDsputs__tmp) (EVar IDsputs_len)))),
             36%positive)::(36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard (fun s => ((eval (EVar IDsputs_status)
             s) >= (eval (ENum (0)) s))%Z)),41%positive)::
             (37%positive,(AGuard (fun s => ((eval (EVar IDsputs_status) s) <
             (eval (ENum (0)) s))%Z)),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,44%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,44%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AGuard (fun s => ((eval (EVar IDsputs_count) s) >
             (eval (EVar IDsputs_len) s))%Z)),48%positive)::
             (46%positive,(AGuard (fun s => ((eval (EVar IDsputs_count) s) <=
             (eval (EVar IDsputs_len) s))%Z)),47%positive)::
             (47%positive,AWeaken,51%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDsputs_count (Some (EVar IDsputs_len))),
             50%positive)::(50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDsputs_len (Some (ESub (EVar IDsputs_len)
             (EVar IDsputs_count)))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDsputs_z (Some (EAdd (ENum (1))
             (EVar IDsputs_z)))),56%positive)::
             (56%positive,AWeaken,14%positive)::nil
|}.

Definition sputs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) <= 0)%Z
    | 4%positive => (-1 * (s IDsputs_len) <= 0 /\ 1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) <= 0)%Z
    | 5%positive => (-1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) <= 0)%Z
    | 6%positive => (-1 * (s IDsputs_len) <= 0 /\ 1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) <= 0)%Z
    | 7%positive => (-1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_z) <= 0)%Z
    | 8%positive => (1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) <= 0)%Z
    | 9%positive => (-1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_z) <= 0)%Z
    | 10%positive => (1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) <= 0 /\ 1 * (s IDsputs_status) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_status) <= 0)%Z
    | 12%positive => (-1 * (s IDsputs_status) <= 0 /\ -1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_z) <= 0)%Z
    | 13%positive => (1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_status) <= 0)%Z
    | 14%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 15%positive => (-1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_len) <= 0)%Z
    | 16%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 18%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 20%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 21%positive => (1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 22%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 23%positive => (1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 24%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 25%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 26%positive => (1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 27%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 28%positive => (1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 29%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 30%positive => (1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 31%positive => (-1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) <= 0)%Z
    | 32%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ 1 * (s IDsputs_count) <= 0)%Z
    | 33%positive => (1 * (s IDsputs_count) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 34%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 35%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 36%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 37%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 38%positive => (-1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_status) + 1 <= 0)%Z
    | 39%positive => (1 * (s IDsputs_status) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 40%positive => (-1 * (s IDsputs_z) <= 0 /\ 1 * (s IDsputs_status) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_status) <= 0)%Z
    | 42%positive => (-1 * (s IDsputs_status) <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 43%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_status) <= 0)%Z
    | 44%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 45%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_count) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDsputs_count) + 1 <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 47%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) + 1 <= 0 /\ 1 * (s IDsputs_count)+ -1 * (s IDsputs_len) <= 0)%Z
    | 48%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_count) + 1 <= 0 /\ -1 * (s IDsputs_count)+ 1 * (s IDsputs_len) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDsputs_count)+ 1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_count) + 1 <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 50%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_count) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDsputs_count) + 1 <= 0 /\ -1 * (s IDsputs_len) + 1 <= 0 /\ -1 * (s IDsputs_z) <= 0)%Z
    | 52%positive => (-1 * (s IDsputs_z) <= 0 /\ -1 * (s IDsputs_count) + 1 <= 0 /\ -1 * (s IDsputs_count)+ -1 * (s IDsputs_len) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 54%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 55%positive => (-1 * (s IDsputs_z) <= 0)%Z
    | 56%positive => (-1 * (s IDsputs_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition sputs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDsputs_wlen)))%Q
    | 2%positive => ((s IDsputs_z) + max0((s IDsputs_wlen)))%Q
    | 3%positive => ((s IDsputs_z) + max0((s IDsputs_wlen)))%Q
    | 4%positive => ((s IDsputs_z) + max0((s IDsputs_wlen)))%Q
    | 5%positive => ((s IDsputs_z) + max0((s IDsputs_wlen)))%Q
    | 6%positive => ((s IDsputs_z) + max0((s IDsputs__tmp)))%Q
    | 7%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 8%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 9%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 10%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 11%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 12%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 13%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 14%positive => (max0((s IDsputs_len)) + max0((s IDsputs_z)))%Q
    | 15%positive => (max0((s IDsputs_len)) + max0((s IDsputs_z)))%Q
    | 16%positive => (max0((s IDsputs_len)) + max0((s IDsputs_z)))%Q
    | 17%positive => ((s IDsputs_len) + max0((s IDsputs_z)))%Q
    | 18%positive => ((s IDsputs_len) + max0((s IDsputs_z)))%Q
    | 19%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 20%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 21%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 22%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 23%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 24%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 25%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 26%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 27%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 28%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 29%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 30%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 31%positive => ((1 # 1) + (s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 32%positive => ((1 # 1) + (s IDsputs_z) + max0(-1 + (s IDsputs_len)))%Q
    | 33%positive => ((s IDsputs_z))%Q
    | 34%positive => ((s IDsputs_z))%Q
    | 35%positive => ((s IDsputs_z))%Q
    | 36%positive => ((s IDsputs_z))%Q
    | 37%positive => ((s IDsputs_z))%Q
    | 38%positive => ((s IDsputs_z))%Q
    | 39%positive => ((s IDsputs_z))%Q
    | 40%positive => ((s IDsputs_z))%Q
    | 41%positive => ((s IDsputs_z))%Q
    | 42%positive => ((s IDsputs_z))%Q
    | 43%positive => ((s IDsputs_z))%Q
    | 44%positive => ((s IDsputs_z))%Q
    | 45%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 46%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 47%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 48%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 49%positive => ((1 # 1) + (s IDsputs_z))%Q
    | 50%positive => ((1 # 1) + (s IDsputs_z)
                      + max0(-(s IDsputs_count) + (s IDsputs_len)))%Q
    | 51%positive => ((1 # 1) + (s IDsputs_z)
                      + max0(-(s IDsputs_count) + (s IDsputs_len)))%Q
    | 52%positive => ((1 # 1) + (s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 53%positive => ((1 # 1) + (s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 54%positive => ((1 # 1) + (s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 55%positive => ((1 # 1) + (s IDsputs_z) + max0((s IDsputs_len)))%Q
    | 56%positive => ((s IDsputs_z) + max0((s IDsputs_len)))%Q
    | _ => (0 # 1)%Q
  end.

Definition sputs_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsputs_len)) (-1
                                                                    + (s IDsputs_len)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsputs_len))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsputs_z)) (0))) (F_max0_ge_0 ((s IDsputs_z)))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsputs_len)) (-1
                                                                    + (s IDsputs_len)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsputs_len));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsputs_z))) (F_check_ge ((s IDsputs_z)) (0))]
    | 16%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsputs_len))) (F_check_ge ((s IDsputs_len)) (0))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsputs_z))) (F_check_ge ((s IDsputs_z)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsputs_len)) (0))) (F_max0_ge_0 ((s IDsputs_len)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsputs_len)) (1)]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsputs_len))]
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
    | 47%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsputs_len)) ((s IDsputs_count));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDsputs_count))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDsputs_count)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDsputs_count)))]
    | 48%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsputs_len)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsputs_len))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsputs_z)) (0))) (F_max0_ge_0 ((s IDsputs_z)))]
    | _ => []
  end.


Theorem sputs_ai_correct:
  forall s p' s', steps (g_start sputs) s (g_edges sputs) p' s' -> sputs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem sputs_pot_correct:
  forall s p' s',
    steps (g_start sputs) s (g_edges sputs) p' s' ->
    (sputs_pot (g_start sputs) s >= sputs_pot p' s')%Q.
Proof.
  check_lp sputs_ai_correct sputs_hints.
Qed.

