Require Import pasta.Pasta.

Notation IDadd_pair_to_block_z := 1%positive.
Notation IDadd_pair_to_block_ch := 2%positive.
Notation IDadd_pair_to_block_i := 3%positive.
Notation IDadd_pair_to_block_s_dref_off108 := 4%positive.
Notation IDadd_pair_to_block_s_dref_off648 := 5%positive.
Notation IDadd_pair_to_block_s_dref_off92 := 6%positive.
Notation IDadd_pair_to_block_s_dref_off96 := 7%positive.
Notation IDadd_pair_to_block_s := 8%positive.
Definition add_pair_to_block : graph := {|
  g_start := 1%positive;
  g_end := 28%positive;
  g_edges := (1%positive,(AAssign IDadd_pair_to_block_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDadd_pair_to_block_ch
             (Some (EVar IDadd_pair_to_block_s_dref_off92))),3%positive)::
             (3%positive,(AAssign IDadd_pair_to_block_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDadd_pair_to_block_i) s) <
             (eval (EVar IDadd_pair_to_block_s_dref_off96) s))%Z)),
             29%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDadd_pair_to_block_i) s) >=
             (eval (EVar IDadd_pair_to_block_s_dref_off96) s))%Z)),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,ANone,21%positive)::(8%positive,ANone,18%positive)::
             (8%positive,ANone,14%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),10%positive)::
             (10%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),11%positive)::
             (11%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,28%positive)::
             (14%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),15%positive)::
             (15%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,28%positive)::
             (18%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,28%positive)::
             (21%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),22%positive)::
             (22%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),23%positive)::
             (23%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),24%positive)::
             (24%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),25%positive)::
             (25%positive,(AAssign IDadd_pair_to_block_s_dref_off108
             (Some (EAdd (EVar IDadd_pair_to_block_s_dref_off108)
             (ENum (1))))),26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDadd_pair_to_block_s_dref_off648 None),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDadd_pair_to_block_i
             (Some (EAdd (EVar IDadd_pair_to_block_i) (ENum (1))))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDadd_pair_to_block_z
             (Some (EAdd (ENum (1)) (EVar IDadd_pair_to_block_z)))),
             36%positive)::(36%positive,AWeaken,6%positive)::nil
|}.

Definition add_pair_to_block_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0)%Z
    | 3%positive => (-1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_z) <= 0)%Z
    | 4%positive => (1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 5%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ 1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_z) <= 0)%Z
    | 6%positive => (-1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 7%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 8%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 9%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 10%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 11%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 12%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 13%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 14%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 15%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 16%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 17%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 18%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 19%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 20%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 21%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 22%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 23%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 24%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 25%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 26%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 27%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0)%Z
    | 28%positive => (-1 * (s IDadd_pair_to_block_i)+ 1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 29%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) + 1 <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 31%positive => (-1 * (s IDadd_pair_to_block_i) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) + 1 <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0 /\ -1 * (s IDadd_pair_to_block_i) <= 0)%Z
    | 33%positive => (-1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDadd_pair_to_block_i) + 1 <= 0 /\ 1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) <= 0)%Z
    | 35%positive => (-1 * (s IDadd_pair_to_block_z) <= 0 /\ 1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDadd_pair_to_block_i) + 1 <= 0 /\ 1 * (s IDadd_pair_to_block_i)+ -1 * (s IDadd_pair_to_block_s_dref_off96) <= 0 /\ -1 * (s IDadd_pair_to_block_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition add_pair_to_block_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDadd_pair_to_block_s_dref_off96)))%Q
    | 2%positive => ((s IDadd_pair_to_block_z)
                     + max0((s IDadd_pair_to_block_s_dref_off96)))%Q
    | 3%positive => ((s IDadd_pair_to_block_z)
                     + max0((s IDadd_pair_to_block_s_dref_off96)))%Q
    | 4%positive => ((s IDadd_pair_to_block_z)
                     + max0(-(s IDadd_pair_to_block_i)
                            + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 5%positive => ((s IDadd_pair_to_block_z)
                     + max0(-(s IDadd_pair_to_block_i)
                            + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 6%positive => ((s IDadd_pair_to_block_z)
                     + max0(-(s IDadd_pair_to_block_i)
                            + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 7%positive => ((s IDadd_pair_to_block_z)
                     + max0(-(s IDadd_pair_to_block_i)
                            + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 8%positive => ((s IDadd_pair_to_block_z)
                     + max0(-(s IDadd_pair_to_block_i)
                            + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 9%positive => ((s IDadd_pair_to_block_z)
                     + max0(-(s IDadd_pair_to_block_i)
                            + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 10%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 11%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 12%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 13%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 14%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 15%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 16%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 17%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 18%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 19%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 20%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 21%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 22%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 23%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 24%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 25%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 26%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 27%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 28%positive => ((s IDadd_pair_to_block_z))%Q
    | 29%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 30%positive => ((1 # 1) + (s IDadd_pair_to_block_z)
                      + max0(-1 - (s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 31%positive => ((1 # 1) + (s IDadd_pair_to_block_z)
                      + max0(-1 - (s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 32%positive => ((1 # 1) + (s IDadd_pair_to_block_z)
                      + max0(-1 - (s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 33%positive => ((1 # 1) + (s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 34%positive => ((1 # 1) + (s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 35%positive => ((1 # 1) + (s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | 36%positive => ((s IDadd_pair_to_block_z)
                      + max0(-(s IDadd_pair_to_block_i)
                             + (s IDadd_pair_to_block_s_dref_off96)))%Q
    | _ => (0 # 1)%Q
  end.

Definition add_pair_to_block_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDadd_pair_to_block_i)
                                                             + (s IDadd_pair_to_block_s_dref_off96)) (-1
                                                                    - (s IDadd_pair_to_block_i)
                                                                    + (s IDadd_pair_to_block_s_dref_off96)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDadd_pair_to_block_i)
                                            + (s IDadd_pair_to_block_s_dref_off96))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDadd_pair_to_block_i)
                                                             + (s IDadd_pair_to_block_s_dref_off96)) (-1
                                                                    - (s IDadd_pair_to_block_i)
                                                                    + (s IDadd_pair_to_block_s_dref_off96)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDadd_pair_to_block_i)
                                            + (s IDadd_pair_to_block_s_dref_off96))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDadd_pair_to_block_i)
                                                             + (s IDadd_pair_to_block_s_dref_off96)) (-1
                                                                    - (s IDadd_pair_to_block_i)
                                                                    + (s IDadd_pair_to_block_s_dref_off96)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDadd_pair_to_block_i)
                                            + (s IDadd_pair_to_block_s_dref_off96))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDadd_pair_to_block_i)
                                                             + (s IDadd_pair_to_block_s_dref_off96)) (-1
                                                                    - (s IDadd_pair_to_block_i)
                                                                    + (s IDadd_pair_to_block_s_dref_off96)));
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDadd_pair_to_block_i)
                                            + (s IDadd_pair_to_block_s_dref_off96))]
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDadd_pair_to_block_i)
                                                     + (s IDadd_pair_to_block_s_dref_off96)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | _ => []
  end.


Theorem add_pair_to_block_ai_correct:
  forall s p' s', steps (g_start add_pair_to_block) s (g_edges add_pair_to_block) p' s' -> add_pair_to_block_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem add_pair_to_block_pot_correct:
  forall s p' s',
    steps (g_start add_pair_to_block) s (g_edges add_pair_to_block) p' s' ->
    (add_pair_to_block_pot (g_start add_pair_to_block) s >= add_pair_to_block_pot p' s')%Q.
Proof.
  check_lp add_pair_to_block_ai_correct add_pair_to_block_hints.
Qed.

