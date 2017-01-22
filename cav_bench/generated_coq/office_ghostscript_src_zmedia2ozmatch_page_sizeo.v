Require Import pasta.Pasta.

Notation IDzmatch_page_size_z := 1%positive.
Notation IDzmatch_page_size__tmp := 2%positive.
Notation IDzmatch_page_size__tmp1 := 3%positive.
Notation IDzmatch_page_size__tmp2 := 4%positive.
Notation IDzmatch_page_size__tmp3 := 5%positive.
Notation IDzmatch_page_size_code := 6%positive.
Notation IDzmatch_page_size_i := 7%positive.
Notation IDzmatch_page_size_nm := 8%positive.
Notation IDzmatch_page_size_best_mismatch := 9%positive.
Notation IDzmatch_page_size_orient := 10%positive.
Notation IDzmatch_page_size_pmat := 11%positive.
Notation IDzmatch_page_size_pmsize := 12%positive.
Notation IDzmatch_page_size_policy := 13%positive.
Notation IDzmatch_page_size_pvmed := 14%positive.
Notation IDzmatch_page_size_pvreq := 15%positive.
Notation IDzmatch_page_size_roll := 16%positive.
Definition zmatch_page_size : graph := {|
  g_start := 1%positive;
  g_end := 43%positive;
  g_edges := (1%positive,(AAssign IDzmatch_page_size_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDzmatch_page_size_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDzmatch_page_size__tmp3
             (Some (EVar IDzmatch_page_size_policy))),5%positive)::
             (5%positive,(AAssign IDzmatch_page_size__tmp2
             (Some (EVar IDzmatch_page_size_orient))),6%positive)::
             (6%positive,(AAssign IDzmatch_page_size__tmp1
             (Some (EVar IDzmatch_page_size_roll))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,12%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDzmatch_page_size__tmp None),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,43%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,17%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDzmatch_page_size__tmp None),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,43%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (18%positive,ANone,25%positive)::
             (19%positive,(AAssign IDzmatch_page_size_nm None),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,29%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDzmatch_page_size_nm) s) =
             (eval (ENum (4)) s))%Z)),28%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDzmatch_page_size_nm) s) <>
             (eval (ENum (4)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDzmatch_page_size__tmp
             (Some (ENum (-15)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,43%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDzmatch_page_size_i (Some (ENum (0)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDzmatch_page_size_i) s) <
             (eval (ENum (4)) s))%Z)),44%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDzmatch_page_size_i) s) >=
             (eval (ENum (4)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDzmatch_page_size_code None),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,40%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDzmatch_page_size__tmp None),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,43%positive)::
             (40%positive,(AAssign IDzmatch_page_size__tmp
             (Some (EVar IDzmatch_page_size_code))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDzmatch_page_size_i
             (Some (EAdd (EVar IDzmatch_page_size_i) (ENum (1))))),
             47%positive)::(47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDzmatch_page_size_z
             (Some (EAdd (ENum (1)) (EVar IDzmatch_page_size_z)))),
             50%positive)::(50%positive,AWeaken,32%positive)::nil
|}.

Definition zmatch_page_size_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 3%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 4%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 5%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 6%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 7%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 8%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 9%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 10%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 11%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 12%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 13%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 14%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 15%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 16%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 17%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 18%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 19%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 20%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 21%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 22%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 23%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 24%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 25%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 26%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size__tmp) + 15 <= 0 /\ -1 * (s IDzmatch_page_size__tmp) + -15 <= 0)%Z
    | 27%positive => (-1 * (s IDzmatch_page_size__tmp) + -15 <= 0 /\ 1 * (s IDzmatch_page_size__tmp) + 15 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 28%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_nm) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_nm) + 4 <= 0)%Z
    | 29%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 30%positive => (1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 31%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_i) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 32%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 33%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 4 <= 0)%Z
    | 34%positive => (-1 * (s IDzmatch_page_size_i) + 4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 35%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 4 <= 0)%Z
    | 36%positive => (-1 * (s IDzmatch_page_size_i) + 4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 37%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 4 <= 0)%Z
    | 38%positive => (-1 * (s IDzmatch_page_size_i) + 4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 39%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 4 <= 0)%Z
    | 40%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 4 <= 0)%Z
    | 41%positive => (-1 * (s IDzmatch_page_size_i) + 4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 42%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 4 <= 0)%Z
    | 43%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 44%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -3 <= 0)%Z
    | 45%positive => (1 * (s IDzmatch_page_size_i) + -3 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) <= 0)%Z
    | 46%positive => (-1 * (s IDzmatch_page_size_i) <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0 /\ 1 * (s IDzmatch_page_size_i) + -3 <= 0)%Z
    | 47%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 1 <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 48%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_i) + 1 <= 0 /\ -1 * (s IDzmatch_page_size_z) <= 0)%Z
    | 49%positive => (-1 * (s IDzmatch_page_size_z) <= 0 /\ -1 * (s IDzmatch_page_size_i) + 1 <= 0 /\ 1 * (s IDzmatch_page_size_i) + -4 <= 0)%Z
    | 50%positive => (1 * (s IDzmatch_page_size_i) + -4 <= 0 /\ -1 * (s IDzmatch_page_size_i) + 1 <= 0 /\ -1 * (s IDzmatch_page_size_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition zmatch_page_size_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 3%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 4%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 5%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 6%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 7%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 8%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 9%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 10%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 11%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 12%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 13%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 14%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 15%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 16%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 17%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 18%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 19%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 20%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 21%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 22%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 23%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 24%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 25%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 26%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 27%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 28%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 29%positive => ((4 # 1) + (s IDzmatch_page_size_z))%Q
    | 30%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 31%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 32%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 33%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 34%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 35%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 36%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 37%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 38%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 39%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 40%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 41%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 42%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 43%positive => ((s IDzmatch_page_size_z))%Q
    | 44%positive => ((s IDzmatch_page_size_z)
                      + max0(4 - (s IDzmatch_page_size_i)))%Q
    | 45%positive => ((4 # 1) - (s IDzmatch_page_size_i)
                      + (s IDzmatch_page_size_z))%Q
    | 46%positive => ((4 # 1) - (s IDzmatch_page_size_i)
                      + (s IDzmatch_page_size_z))%Q
    | 47%positive => ((5 # 1) - (s IDzmatch_page_size_i)
                      + (s IDzmatch_page_size_z))%Q
    | 48%positive => ((5 # 1) - (s IDzmatch_page_size_i)
                      + (s IDzmatch_page_size_z))%Q
    | 49%positive => ((5 # 1) - (s IDzmatch_page_size_i)
                      + (s IDzmatch_page_size_z))%Q
    | 50%positive => ((4 # 1) - (s IDzmatch_page_size_i)
                      + (s IDzmatch_page_size_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition zmatch_page_size_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-4 0*) F_one]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-4 0*) F_one]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDzmatch_page_size_z))) (F_check_ge ((s IDzmatch_page_size_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDzmatch_page_size_z)) (0))) (F_max0_ge_0 ((s IDzmatch_page_size_z)))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-4 0*) F_one]
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
    | 39%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzmatch_page_size_i)) (3
                                                                    - (s IDzmatch_page_size_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDzmatch_page_size_i))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDzmatch_page_size_i)) (3
                                                                    - (s IDzmatch_page_size_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDzmatch_page_size_i))]
    | 43%positive => []
    | 44%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                  - (s IDzmatch_page_size_i))) (F_check_ge (4
                                                                    - (s IDzmatch_page_size_i)) (0))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDzmatch_page_size_i)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDzmatch_page_size_i)))]
    | _ => []
  end.


Theorem zmatch_page_size_ai_correct:
  forall s p' s', steps (g_start zmatch_page_size) s (g_edges zmatch_page_size) p' s' -> zmatch_page_size_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem zmatch_page_size_pot_correct:
  forall s p' s',
    steps (g_start zmatch_page_size) s (g_edges zmatch_page_size) p' s' ->
    (zmatch_page_size_pot (g_start zmatch_page_size) s >= zmatch_page_size_pot p' s')%Q.
Proof.
  check_lp zmatch_page_size_ai_correct zmatch_page_size_hints.
Qed.

