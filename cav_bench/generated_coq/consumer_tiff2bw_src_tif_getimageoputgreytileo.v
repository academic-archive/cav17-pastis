Require Import pasta.Pasta.

Notation IDputgreytile_z := 1%positive.
Notation IDputgreytile__tmp := 2%positive.
Notation IDputgreytile__tmp1 := 3%positive.
Notation IDputgreytile__tmp2 := 4%positive.
Notation IDputgreytile__tmp3 := 5%positive.
Notation IDputgreytile__tmp4 := 6%positive.
Notation IDputgreytile__tmp5 := 7%positive.
Notation IDputgreytile_cp := 8%positive.
Notation IDputgreytile_fromskew := 9%positive.
Notation IDputgreytile_h := 10%positive.
Notation IDputgreytile_img := 11%positive.
Notation IDputgreytile_pp := 12%positive.
Notation IDputgreytile_toskew := 13%positive.
Notation IDputgreytile_w := 14%positive.
Notation IDputgreytile_x := 15%positive.
Notation IDputgreytile_y := 16%positive.
Definition putgreytile : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDputgreytile_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDputgreytile__tmp1)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDputgreytile__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDputgreytile__tmp1
             (Some (EVar IDputgreytile_x))),6%positive)::
             (6%positive,(AAssign IDputgreytile__tmp5
             (Some (EVar IDputgreytile_y))),7%positive)::
             (7%positive,(AAssign IDputgreytile__tmp2
             (Some (EVar IDputgreytile_w))),8%positive)::
             (8%positive,(AAssign IDputgreytile__tmp
             (Some (EVar IDputgreytile_h))),9%positive)::
             (9%positive,(AAssign IDputgreytile__tmp4
             (Some (EVar IDputgreytile_fromskew))),10%positive)::
             (10%positive,(AAssign IDputgreytile__tmp3
             (Some (EVar IDputgreytile_toskew))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDputgreytile__tmp
             (Some (EAdd (EVar IDputgreytile__tmp) (ENum (-1))))),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDputgreytile__tmp)
             s) > (eval (ENum (0)) s))%Z)),17%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDputgreytile__tmp)
             s) <= (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDputgreytile__tmp1
             (Some (EVar IDputgreytile__tmp2))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDputgreytile__tmp1
             (Some (EAdd (EVar IDputgreytile__tmp1) (ENum (-1))))),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDputgreytile__tmp1)
             s) > (eval (ENum (0)) s))%Z)),27%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDputgreytile__tmp1)
             s) <= (eval (ENum (0)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDputgreytile_z (Some (EAdd (ENum (1))
             (EVar IDputgreytile_z)))),12%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDputgreytile_z (Some (EAdd (ENum (1))
             (EVar IDputgreytile_z)))),20%positive)::nil
|}.

Definition putgreytile_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile_z) <= 0)%Z
    | 3%positive => (-1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp1) <= 0)%Z
    | 4%positive => (-1 * (s IDputgreytile__tmp1) <= 0 /\ 1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDputgreytile__tmp) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp1) <= 0)%Z
    | 6%positive => (1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDputgreytile__tmp) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile_z) <= 0)%Z
    | 8%positive => (1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile_z) <= 0)%Z
    | 10%positive => (1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile_z) <= 0)%Z
    | 11%positive => (-1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile_z) <= 0)%Z
    | 12%positive => (-1 * (s IDputgreytile_z) <= 0)%Z
    | 13%positive => (-1 * (s IDputgreytile_z) <= 0)%Z
    | 14%positive => (-1 * (s IDputgreytile_z) <= 0)%Z
    | 15%positive => (-1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile__tmp) <= 0)%Z
    | 16%positive => (1 * (s IDputgreytile__tmp) <= 0 /\ -1 * (s IDputgreytile_z) <= 0)%Z
    | 17%positive => (-1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDputgreytile__tmp) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0)%Z
    | 19%positive => (-1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDputgreytile__tmp) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0)%Z
    | 22%positive => (-1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDputgreytile__tmp) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile__tmp1) <= 0)%Z
    | 24%positive => (1 * (s IDputgreytile__tmp1) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDputgreytile__tmp) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ 1 * (s IDputgreytile__tmp1) <= 0)%Z
    | 26%positive => (1 * (s IDputgreytile__tmp1) <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDputgreytile__tmp) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp1) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDputgreytile__tmp1) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDputgreytile__tmp) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp1) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDputgreytile__tmp1) + 1 <= 0 /\ -1 * (s IDputgreytile_z) <= 0 /\ -1 * (s IDputgreytile__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition putgreytile_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 2%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 3%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 4%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 5%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 6%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 7%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)) * max0(-1
                                                             + (s IDputgreytile_w)))%Q
    | 8%positive => ((s IDputgreytile_z)
                     + max0(-1 + (s IDputgreytile__tmp2)) * max0(-1
                                                                 + (s IDputgreytile_h))
                     + max0(-1 + (s IDputgreytile_h)))%Q
    | 9%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile__tmp))
                     + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                + (s IDputgreytile__tmp2)))%Q
    | 10%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile__tmp))
                      + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                 + (s IDputgreytile__tmp2)))%Q
    | 11%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile__tmp))
                      + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                 + (s IDputgreytile__tmp2)))%Q
    | 12%positive => ((s IDputgreytile_z) + max0(-1 + (s IDputgreytile__tmp))
                      + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                 + (s IDputgreytile__tmp2)))%Q
    | 13%positive => ((s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp2)) * max0((s IDputgreytile__tmp))
                      + max0((s IDputgreytile__tmp)))%Q
    | 14%positive => ((s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp2)) * max0((s IDputgreytile__tmp))
                      + max0((s IDputgreytile__tmp)))%Q
    | 15%positive => ((s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp2)) * max0((s IDputgreytile__tmp))
                      + max0((s IDputgreytile__tmp)))%Q
    | 16%positive => ((s IDputgreytile_z))%Q
    | 17%positive => ((s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp2)) * max0((s IDputgreytile__tmp))
                      + max0((s IDputgreytile__tmp)))%Q
    | 18%positive => ((1 # 1)
                      + (s IDputgreytile__tmp) * max0(-1
                                                      + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp)))%Q
    | 19%positive => ((1 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2)))%Q
    | 20%positive => ((1 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2)))%Q
    | 21%positive => ((1 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0((s IDputgreytile__tmp1))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0((s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2))
                      + (1 # 2) * max0((s IDputgreytile__tmp1)))%Q
    | 22%positive => ((1 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2))
                      + max0((s IDputgreytile__tmp1)))%Q
    | 23%positive => ((1 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2))
                      + max0((s IDputgreytile__tmp1)))%Q
    | 24%positive => ((1 # 1) + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                 + (s IDputgreytile__tmp2)))%Q
    | 25%positive => ((1 # 1) + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                 + (s IDputgreytile__tmp2)))%Q
    | 26%positive => ((1 # 1) + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      + max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                 + (s IDputgreytile__tmp2)))%Q
    | 27%positive => ((1 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2))
                      + max0((s IDputgreytile__tmp1)))%Q
    | 28%positive => ((2 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2)))%Q
    | 29%positive => ((2 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2)))%Q
    | 30%positive => ((2 # 1)
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp1))
                      + (1 # 2) * (s IDputgreytile__tmp) * max0(-1
                                                                + (s IDputgreytile__tmp2))
                      + (s IDputgreytile_z)
                      + max0(-1 + (s IDputgreytile__tmp))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp1))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp)) * max0(-1
                                                                    + (s IDputgreytile__tmp2))
                      + (1 # 2) * max0(-1 + (s IDputgreytile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDputgreytile__tmp2)))%Q
    | _ => (0 # 1)%Q
  end.

Definition putgreytile_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDputgreytile__tmp)) (-1
                                                                    + (s IDputgreytile__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDputgreytile__tmp));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp2))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputgreytile__tmp))) (F_check_ge (0) (0)))]
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_pre_decrement ((s IDputgreytile__tmp)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputgreytile__tmp))) (F_check_ge ((s IDputgreytile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp2))) (F_check_ge (0) (0)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputgreytile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputgreytile__tmp1))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDputgreytile__tmp))) (F_check_ge (-1
                                                                    + (s IDputgreytile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp1))) (F_check_ge (0) (0)))]
    | 22%positive => []
    | 23%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputgreytile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp1))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputgreytile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputgreytile__tmp2))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDputgreytile__tmp1))) (F_check_ge (0) (0))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_max0_pre_decrement ((s IDputgreytile__tmp1)) (1)]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | _ => []
  end.


Theorem putgreytile_ai_correct:
  forall s p' s', steps (g_start putgreytile) s (g_edges putgreytile) p' s' -> putgreytile_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem putgreytile_pot_correct:
  forall s p' s',
    steps (g_start putgreytile) s (g_edges putgreytile) p' s' ->
    (putgreytile_pot (g_start putgreytile) s >= putgreytile_pot p' s')%Q.
Proof.
  check_lp putgreytile_ai_correct putgreytile_hints.
Qed.

