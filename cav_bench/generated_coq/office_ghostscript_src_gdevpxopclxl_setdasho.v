Require Import pasta.Pasta.

Notation IDpclxl_setdash_z := 1%positive.
Notation IDpclxl_setdash__tmp := 2%positive.
Notation IDpclxl_setdash__tmp1 := 3%positive.
Notation IDpclxl_setdash_i := 4%positive.
Notation IDpclxl_setdash_count := 5%positive.
Notation IDpclxl_setdash_offset := 6%positive.
Notation IDpclxl_setdash_pattern := 7%positive.
Notation IDpclxl_setdash_vdev := 8%positive.
Definition pclxl_setdash : graph := {|
  g_start := 1%positive;
  g_end := 36%positive;
  g_edges := (1%positive,(AAssign IDpclxl_setdash_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpclxl_setdash__tmp
             (Some (EVar IDpclxl_setdash_count))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash__tmp)
             s) = (eval (ENum (0)) s))%Z)),31%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash__tmp)
             s) <> (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash__tmp)
             s) > (eval (ENum (255)) s))%Z)),27%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash__tmp)
             s) <= (eval (ENum (255)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDpclxl_setdash_i (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash_i)
             s) < (eval (EVar IDpclxl_setdash__tmp) s))%Z)),20%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDpclxl_setdash_i)
             s) >= (eval (EVar IDpclxl_setdash__tmp) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,18%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,33%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDpclxl_setdash_i
             (Some (EAdd (EVar IDpclxl_setdash_i) (ENum (1))))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDpclxl_setdash_z (Some (EAdd (ENum (1))
             (EVar IDpclxl_setdash_z)))),26%positive)::
             (26%positive,AWeaken,14%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDpclxl_setdash__tmp1
             (Some (ENum (-13)))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,36%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDpclxl_setdash__tmp1 (Some (ENum (0)))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::nil
|}.

Definition pclxl_setdash_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 4%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDpclxl_setdash__tmp) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 6%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 8%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 10%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0)%Z
    | 11%positive => (1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 12%positive => (1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ 1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 13%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 14%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0)%Z
    | 15%positive => (1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 16%positive => (1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0)%Z
    | 17%positive => (1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 18%positive => (1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0)%Z
    | 19%positive => (1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 20%positive => (1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) + 1 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0)%Z
    | 22%positive => (1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) + 1 <= 0 /\ -1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 24%positive => (-1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_i) + 1 <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 25%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) + 1 <= 0 /\ -1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 26%positive => (-1 * (s IDpclxl_setdash__tmp)+ 1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_i) + 1 <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_z) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash__tmp) + 256 <= 0)%Z
    | 28%positive => (-1 * (s IDpclxl_setdash__tmp) + 256 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 29%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash__tmp) + 256 <= 0 /\ 1 * (s IDpclxl_setdash__tmp1) + 13 <= 0 /\ -1 * (s IDpclxl_setdash__tmp1) + -13 <= 0)%Z
    | 30%positive => (-1 * (s IDpclxl_setdash__tmp1) + -13 <= 0 /\ 1 * (s IDpclxl_setdash__tmp1) + 13 <= 0 /\ -1 * (s IDpclxl_setdash__tmp) + 256 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 31%positive => (-1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) <= 0 /\ -1 * (s IDpclxl_setdash__tmp) <= 0)%Z
    | 32%positive => (-1 * (s IDpclxl_setdash__tmp) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ 1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0)%Z
    | 33%positive => (1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 34%positive => (-1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ 1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp1) <= 0 /\ -1 * (s IDpclxl_setdash__tmp1) <= 0)%Z
    | 35%positive => (-1 * (s IDpclxl_setdash__tmp1) <= 0 /\ 1 * (s IDpclxl_setdash__tmp1) <= 0 /\ 1 * (s IDpclxl_setdash__tmp)+ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp) + -255 <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0)%Z
    | 36%positive => (-1 * (s IDpclxl_setdash__tmp1) + -13 <= 0 /\ -1 * (s IDpclxl_setdash_z) <= 0 /\ -1 * (s IDpclxl_setdash_i) <= 0 /\ 1 * (s IDpclxl_setdash__tmp1) <= 0)%Z
    | _ => False
  end.

Definition pclxl_setdash_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpclxl_setdash_count)))%Q
    | 2%positive => (max0((s IDpclxl_setdash_count))
                     + max0((s IDpclxl_setdash_z)))%Q
    | 3%positive => (max0((s IDpclxl_setdash_count))
                     + max0((s IDpclxl_setdash_z)))%Q
    | 4%positive => (max0((s IDpclxl_setdash_count))
                     + max0((s IDpclxl_setdash_z)))%Q
    | 5%positive => ((s IDpclxl_setdash_z) + max0((s IDpclxl_setdash_count)))%Q
    | 6%positive => ((s IDpclxl_setdash_z) + max0((s IDpclxl_setdash__tmp)))%Q
    | 7%positive => ((s IDpclxl_setdash_z) + max0((s IDpclxl_setdash__tmp)))%Q
    | 8%positive => ((s IDpclxl_setdash_z) + max0((s IDpclxl_setdash__tmp)))%Q
    | 9%positive => (max0((s IDpclxl_setdash__tmp))
                     + max0((s IDpclxl_setdash_z)))%Q
    | 10%positive => (max0((s IDpclxl_setdash__tmp))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 11%positive => (max0((s IDpclxl_setdash__tmp))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 12%positive => (max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 13%positive => (max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 14%positive => ((s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 15%positive => ((s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 16%positive => ((s IDpclxl_setdash_z))%Q
    | 17%positive => ((s IDpclxl_setdash_z))%Q
    | 18%positive => ((s IDpclxl_setdash_z))%Q
    | 19%positive => ((s IDpclxl_setdash_z))%Q
    | 20%positive => ((s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 21%positive => ((1 # 1) + (s IDpclxl_setdash_z)
                      + max0(-1 + (s IDpclxl_setdash__tmp)
                             - (s IDpclxl_setdash_i)))%Q
    | 22%positive => ((1 # 1) + (s IDpclxl_setdash_z)
                      + max0(-1 + (s IDpclxl_setdash__tmp)
                             - (s IDpclxl_setdash_i)))%Q
    | 23%positive => ((1 # 1) + (s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 24%positive => ((1 # 1) + (s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 25%positive => ((1 # 1) + (s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 26%positive => ((s IDpclxl_setdash_z)
                      + max0((s IDpclxl_setdash__tmp) - (s IDpclxl_setdash_i)))%Q
    | 27%positive => (max0((s IDpclxl_setdash__tmp))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 28%positive => (max0((s IDpclxl_setdash__tmp))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 29%positive => (-(1 # 1) + max0((s IDpclxl_setdash__tmp))
                      + (1 # 13) * max0(-(s IDpclxl_setdash__tmp1))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 30%positive => (-(1 # 1) + max0((s IDpclxl_setdash__tmp))
                      + (1 # 13) * max0(-(s IDpclxl_setdash__tmp1))
                      + max0((s IDpclxl_setdash_z)))%Q
    | 31%positive => ((s IDpclxl_setdash_z) + max0((s IDpclxl_setdash__tmp)))%Q
    | 32%positive => ((s IDpclxl_setdash_z))%Q
    | 33%positive => ((s IDpclxl_setdash_z))%Q
    | 34%positive => ((s IDpclxl_setdash_z))%Q
    | 35%positive => ((s IDpclxl_setdash_z))%Q
    | 36%positive => ((s IDpclxl_setdash_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pclxl_setdash_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpclxl_setdash_z))) (F_check_ge ((s IDpclxl_setdash_z)) (0))]
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpclxl_setdash_z)) (0))) (F_max0_ge_0 ((s IDpclxl_setdash_z)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpclxl_setdash_z))) (F_check_ge ((s IDpclxl_setdash_z)) (0))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpclxl_setdash__tmp)
                                                             - (s IDpclxl_setdash_i)) (-1
                                                                    + (s IDpclxl_setdash__tmp)
                                                                    - (s IDpclxl_setdash_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDpclxl_setdash__tmp)
                                                                 - (s IDpclxl_setdash_i))) (F_check_ge (0) (0))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_pre_decrement ((s IDpclxl_setdash__tmp)
                                                     - (s IDpclxl_setdash_i)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpclxl_setdash_z))) (F_check_ge ((s IDpclxl_setdash_z)) (0));
                      (*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDpclxl_setdash__tmp1))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpclxl_setdash__tmp))) (F_check_ge ((s IDpclxl_setdash__tmp)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDpclxl_setdash__tmp))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDpclxl_setdash__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDpclxl_setdash__tmp)))]
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDpclxl_setdash__tmp))) (F_check_ge (0) (0))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | _ => []
  end.


Theorem pclxl_setdash_ai_correct:
  forall s p' s', steps (g_start pclxl_setdash) s (g_edges pclxl_setdash) p' s' -> pclxl_setdash_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pclxl_setdash_pot_correct:
  forall s p' s',
    steps (g_start pclxl_setdash) s (g_edges pclxl_setdash) p' s' ->
    (pclxl_setdash_pot (g_start pclxl_setdash) s >= pclxl_setdash_pot p' s')%Q.
Proof.
  check_lp pclxl_setdash_ai_correct pclxl_setdash_hints.
Qed.

