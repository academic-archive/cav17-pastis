Require Import pasta.Pasta.

Notation IDcalculate_dst_contrib_z := 1%positive.
Notation IDcalculate_dst_contrib__tmp := 2%positive.
Notation IDcalculate_dst_contrib_first_index_mod := 3%positive.
Notation IDcalculate_dst_contrib_i := 4%positive.
Notation IDcalculate_dst_contrib_last_index := 5%positive.
Notation IDcalculate_dst_contrib_row_size := 6%positive.
Notation IDcalculate_dst_contrib_ss := 7%positive.
Notation IDcalculate_dst_contrib_y := 8%positive.
Definition calculate_dst_contrib : graph := {|
  g_start := 1%positive;
  g_end := 18%positive;
  g_edges := (1%positive,(AAssign IDcalculate_dst_contrib_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcalculate_dst_contrib__tmp
             (Some (EVar IDcalculate_dst_contrib_y))),3%positive)::
             (3%positive,(AAssign IDcalculate_dst_contrib_row_size None),
             4%positive)::
             (4%positive,(AAssign IDcalculate_dst_contrib_last_index None),
             5%positive)::
             (5%positive,(AAssign IDcalculate_dst_contrib_first_index_mod
             None),6%positive)::
             (6%positive,(AAssign IDcalculate_dst_contrib_last_index None),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_last_index) s) <
             (eval (EVar IDcalculate_dst_contrib_first_index_mod) s))%Z)),
             10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_last_index) s) >=
             (eval (EVar IDcalculate_dst_contrib_first_index_mod) s))%Z)),
             9%positive)::(9%positive,AWeaken,18%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDcalculate_dst_contrib_i
             (Some (ENum (0)))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_i) s) <
             (eval (ENum (8)) s))%Z)),19%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_i) s) >=
             (eval (ENum (8)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_i) s) <=
             (eval (EVar IDcalculate_dst_contrib_last_index) s))%Z)),
             28%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_i) s) >
             (eval (EVar IDcalculate_dst_contrib_last_index) s))%Z)),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_i) s) >=
             (eval (EVar IDcalculate_dst_contrib_first_index_mod) s))%Z)),
             25%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcalculate_dst_contrib_i) s) <
             (eval (EVar IDcalculate_dst_contrib_first_index_mod) s))%Z)),
             23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,ANone,27%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,30%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDcalculate_dst_contrib_i
             (Some (EAdd (EVar IDcalculate_dst_contrib_i) (ENum (1))))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDcalculate_dst_contrib_z
             (Some (EAdd (ENum (1)) (EVar IDcalculate_dst_contrib_z)))),
             35%positive)::(35%positive,AWeaken,14%positive)::nil
|}.

Definition calculate_dst_contrib_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 4%positive => (1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 6%positive => (1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 8%positive => (1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_first_index_mod)+ -1 * (s IDcalculate_dst_contrib_last_index) <= 0)%Z
    | 10%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0)%Z
    | 13%positive => (-1 * (s IDcalculate_dst_contrib_i) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -8 <= 0)%Z
    | 15%positive => (1 * (s IDcalculate_dst_contrib_i) + -8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) + 8 <= 0)%Z
    | 16%positive => (-1 * (s IDcalculate_dst_contrib_i) + 8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -8 <= 0)%Z
    | 17%positive => (1 * (s IDcalculate_dst_contrib_i) + -8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) + 8 <= 0)%Z
    | 18%positive => (-1 * (s IDcalculate_dst_contrib_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0)%Z
    | 20%positive => (1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_i) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0)%Z
    | 25%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_first_index_mod)+ -1 * (s IDcalculate_dst_contrib_i) <= 0)%Z
    | 26%positive => (1 * (s IDcalculate_dst_contrib_first_index_mod)+ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i)+ -1 * (s IDcalculate_dst_contrib_last_index) <= 0)%Z
    | 29%positive => (1 * (s IDcalculate_dst_contrib_i)+ -1 * (s IDcalculate_dst_contrib_last_index) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -7 <= 0)%Z
    | 31%positive => (1 * (s IDcalculate_dst_contrib_i) + -7 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDcalculate_dst_contrib_i) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDcalculate_dst_contrib_i) + 1 <= 0 /\ 1 * (s IDcalculate_dst_contrib_i) + -8 <= 0 /\ -1 * (s IDcalculate_dst_contrib_first_index_mod)+ 1 * (s IDcalculate_dst_contrib_last_index) + 1 <= 0 /\ -1 * (s IDcalculate_dst_contrib_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition calculate_dst_contrib_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 3%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 4%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 5%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 6%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 7%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 8%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 9%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 10%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 11%positive => ((8 # 1) + (s IDcalculate_dst_contrib_z))%Q
    | 12%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 13%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 14%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 15%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 16%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 17%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 18%positive => ((s IDcalculate_dst_contrib_z))%Q
    | 19%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 20%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 21%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 22%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 23%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 24%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(7 - (s IDcalculate_dst_contrib_i)))%Q
    | 25%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 26%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(7 - (s IDcalculate_dst_contrib_i)))%Q
    | 27%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(7 - (s IDcalculate_dst_contrib_i)))%Q
    | 28%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 29%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(7 - (s IDcalculate_dst_contrib_i)))%Q
    | 30%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(7 - (s IDcalculate_dst_contrib_i)))%Q
    | 31%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(7 - (s IDcalculate_dst_contrib_i)))%Q
    | 32%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 33%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 34%positive => ((1 # 1) + (s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | 35%positive => ((s IDcalculate_dst_contrib_z)
                      + max0(8 - (s IDcalculate_dst_contrib_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition calculate_dst_contrib_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-8 0*) F_one]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDcalculate_dst_contrib_i)) (7
                                                                    - (s IDcalculate_dst_contrib_i)));
                      (*-1 0*) F_max0_ge_0 (7 - (s IDcalculate_dst_contrib_i))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_pre_decrement (8
                                                     - (s IDcalculate_dst_contrib_i)) (1)]
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (8
                                                     - (s IDcalculate_dst_contrib_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_pre_decrement (8
                                                     - (s IDcalculate_dst_contrib_i)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | _ => []
  end.


Theorem calculate_dst_contrib_ai_correct:
  forall s p' s', steps (g_start calculate_dst_contrib) s (g_edges calculate_dst_contrib) p' s' -> calculate_dst_contrib_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem calculate_dst_contrib_pot_correct:
  forall s p' s',
    steps (g_start calculate_dst_contrib) s (g_edges calculate_dst_contrib) p' s' ->
    (calculate_dst_contrib_pot (g_start calculate_dst_contrib) s >= calculate_dst_contrib_pot p' s')%Q.
Proof.
  check_lp calculate_dst_contrib_ai_correct calculate_dst_contrib_hints.
Qed.

