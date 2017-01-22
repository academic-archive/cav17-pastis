Require Import pasta.Pasta.

Notation IDputRGBAAseparate8bittile_z := 1%positive.
Notation IDputRGBAAseparate8bittile__tmp := 2%positive.
Notation IDputRGBAAseparate8bittile__tmp1 := 3%positive.
Notation IDputRGBAAseparate8bittile__tmp2 := 4%positive.
Notation IDputRGBAAseparate8bittile__tmp3 := 5%positive.
Notation IDputRGBAAseparate8bittile__tmp4 := 6%positive.
Notation IDputRGBAAseparate8bittile__tmp5 := 7%positive.
Notation IDputRGBAAseparate8bittile__x := 8%positive.
Notation IDputRGBAAseparate8bittile_a := 9%positive.
Notation IDputRGBAAseparate8bittile_b := 10%positive.
Notation IDputRGBAAseparate8bittile_cp := 11%positive.
Notation IDputRGBAAseparate8bittile_fromskew := 12%positive.
Notation IDputRGBAAseparate8bittile_g := 13%positive.
Notation IDputRGBAAseparate8bittile_h := 14%positive.
Notation IDputRGBAAseparate8bittile_img := 15%positive.
Notation IDputRGBAAseparate8bittile_r := 16%positive.
Notation IDputRGBAAseparate8bittile_toskew := 17%positive.
Notation IDputRGBAAseparate8bittile_w := 18%positive.
Notation IDputRGBAAseparate8bittile_x := 19%positive.
Notation IDputRGBAAseparate8bittile_y := 20%positive.
Definition putRGBAAseparate8bittile : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDputRGBAAseparate8bittile_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__x) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDputRGBAAseparate8bittile__tmp5
             (Some (EVar IDputRGBAAseparate8bittile_x))),6%positive)::
             (6%positive,(AAssign IDputRGBAAseparate8bittile__tmp4
             (Some (EVar IDputRGBAAseparate8bittile_y))),7%positive)::
             (7%positive,(AAssign IDputRGBAAseparate8bittile__tmp1
             (Some (EVar IDputRGBAAseparate8bittile_w))),8%positive)::
             (8%positive,(AAssign IDputRGBAAseparate8bittile__tmp
             (Some (EVar IDputRGBAAseparate8bittile_h))),9%positive)::
             (9%positive,(AAssign IDputRGBAAseparate8bittile__tmp3
             (Some (EVar IDputRGBAAseparate8bittile_fromskew))),10%positive)::
             (10%positive,(AAssign IDputRGBAAseparate8bittile__tmp2
             (Some (EVar IDputRGBAAseparate8bittile_toskew))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDputRGBAAseparate8bittile__tmp
             (Some (EAdd (EVar IDputRGBAAseparate8bittile__tmp)
             (ENum (-1))))),13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__tmp) s) >
             (eval (ENum (0)) s))%Z)),17%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__tmp) s) <=
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDputRGBAAseparate8bittile__x
             (Some (EVar IDputRGBAAseparate8bittile__tmp1))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__x) s) >=
             (eval (ENum (8)) s))%Z)),38%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__x) s) <
             (eval (ENum (8)) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__x) s) >
             (eval (ENum (0)) s))%Z)),25%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBAAseparate8bittile__x) s) <=
             (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,35%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,34%positive)::
             (26%positive,ANone,27%positive)::
             (26%positive,ANone,28%positive)::
             (26%positive,ANone,29%positive)::
             (26%positive,ANone,30%positive)::
             (26%positive,ANone,31%positive)::
             (26%positive,ANone,32%positive)::
             (26%positive,ANone,33%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDputRGBAAseparate8bittile_z
             (Some (EAdd (ENum (1)) (EVar IDputRGBAAseparate8bittile_z)))),
             12%positive)::(38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDputRGBAAseparate8bittile__x
             (Some (ESub (EVar IDputRGBAAseparate8bittile__x) (ENum (8))))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDputRGBAAseparate8bittile_z
             (Some (EAdd (ENum (1)) (EVar IDputRGBAAseparate8bittile_z)))),
             44%positive)::(44%positive,AWeaken,21%positive)::nil
|}.

Definition putRGBAAseparate8bittile_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 3%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 4%positive => (-1 * (s IDputRGBAAseparate8bittile__x) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 6%positive => (-1 * (s IDputRGBAAseparate8bittile__x) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 8%positive => (-1 * (s IDputRGBAAseparate8bittile__x) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 10%positive => (-1 * (s IDputRGBAAseparate8bittile__x) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 11%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 12%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 13%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 14%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 15%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__tmp) <= 0)%Z
    | 16%positive => (1 * (s IDputRGBAAseparate8bittile__tmp) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 17%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 19%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 21%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 25%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDputRGBAAseparate8bittile__x) + 1 <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0)%Z
    | 36%positive => (1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ 1 * (s IDputRGBAAseparate8bittile__x) + -7 <= 0)%Z
    | 38%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 8 <= 0)%Z
    | 39%positive => (-1 * (s IDputRGBAAseparate8bittile__x) + 8 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) + 8 <= 0)%Z
    | 41%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 42%positive => (-1 * (s IDputRGBAAseparate8bittile__x) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) <= 0)%Z
    | 43%positive => (-1 * (s IDputRGBAAseparate8bittile_z) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__x) <= 0)%Z
    | 44%positive => (-1 * (s IDputRGBAAseparate8bittile__x) <= 0 /\ -1 * (s IDputRGBAAseparate8bittile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBAAseparate8bittile_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition putRGBAAseparate8bittile_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 2%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 3%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 4%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 5%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 6%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 7%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile_w)))%Q
    | 8%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile_h))
                     + (1 # 8) * max0(-1 + (s IDputRGBAAseparate8bittile_h)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 9%positive => ((s IDputRGBAAseparate8bittile_z)
                     + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                     + (1 # 8) * max0(-1
                                      + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 10%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 11%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 12%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 13%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 14%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 15%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 16%positive => ((s IDputRGBAAseparate8bittile_z))%Q
    | 17%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 18%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 19%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 20%positive => ((s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 21%positive => ((1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 22%positive => ((1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 23%positive => ((s IDputRGBAAseparate8bittile__tmp)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * (s IDputRGBAAseparate8bittile__tmp)^2
                      + (0 # 1) * (s IDputRGBAAseparate8bittile__x) * max0(7
                                                                    - (s IDputRGBAAseparate8bittile__x))
                      + (s IDputRGBAAseparate8bittile_z)
                      - (3 # 103) * max0(7
                                         - (s IDputRGBAAseparate8bittile__x))
                      + (0 # 1) * max0(7 - (s IDputRGBAAseparate8bittile__x))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 24%positive => ((s IDputRGBAAseparate8bittile__tmp)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * (s IDputRGBAAseparate8bittile__tmp)^2
                      + (0 # 1) * (s IDputRGBAAseparate8bittile__x) * max0(7
                                                                    - (s IDputRGBAAseparate8bittile__x))
                      + (s IDputRGBAAseparate8bittile_z)
                      - (3 # 103) * max0(7
                                         - (s IDputRGBAAseparate8bittile__x))
                      + (0 # 1) * max0(7 - (s IDputRGBAAseparate8bittile__x))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 25%positive => ((s IDputRGBAAseparate8bittile__tmp)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * (s IDputRGBAAseparate8bittile__tmp)^2
                      + (0 # 1) * (s IDputRGBAAseparate8bittile__x) * max0(7
                                                                    - (s IDputRGBAAseparate8bittile__x))
                      + (s IDputRGBAAseparate8bittile_z)
                      - (3 # 103) * max0(7
                                         - (s IDputRGBAAseparate8bittile__x))
                      + (0 # 1) * max0(7 - (s IDputRGBAAseparate8bittile__x))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 26%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 27%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 28%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 29%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 30%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 31%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 32%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 33%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 34%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 35%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 36%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 37%positive => ((1 # 1) + (s IDputRGBAAseparate8bittile_z)
                      + max0(-1 + (s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0(-1
                                       + (s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 38%positive => ((1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 39%positive => ((1 # 1)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + (1 # 8) * max0(-8 + (s IDputRGBAAseparate8bittile__x))
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 40%positive => ((1 # 1)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + (1 # 8) * max0(-8 + (s IDputRGBAAseparate8bittile__x))
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1)))%Q
    | 41%positive => ((1 # 1)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 42%positive => ((1 # 1)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 43%positive => ((1 # 1)
                      + (1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | 44%positive => ((1 # 3) * (s IDputRGBAAseparate8bittile__tmp) * max0((s IDputRGBAAseparate8bittile__tmp))
                      + (s IDputRGBAAseparate8bittile_z)
                      + max0((s IDputRGBAAseparate8bittile__tmp))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp)) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      - (1 # 3) * max0((s IDputRGBAAseparate8bittile__tmp))^2
                      - (1 # 8) * max0((s IDputRGBAAseparate8bittile__tmp1))
                      + (1 # 8) * max0((s IDputRGBAAseparate8bittile__x)))%Q
    | _ => (0 # 1)%Q
  end.

Definition putRGBAAseparate8bittile_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDputRGBAAseparate8bittile__tmp));
                      (*-0.125 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp1))) (F_check_ge (0) (0)))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (0) (0)))]
    | 21%positive => []
    | 22%positive => [(*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x)))) (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*0 0.125*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp1))) (F_check_ge (0) (0)));
                      (*0 0.666667*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0));
                      (*0 0.333333*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp)))]
    | 23%positive => []
    | 24%positive => [(*-0.333333 0*) F_max0_pre_decrement ((s IDputRGBAAseparate8bittile__tmp)) (1);
                      (*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDputRGBAAseparate8bittile__x)) (-8
                                                                    + (s IDputRGBAAseparate8bittile__x)));
                      (*-0.125 0*) F_max0_ge_0 (-8
                                                + (s IDputRGBAAseparate8bittile__x));
                      (*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)) (0));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (0) (0)));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (0) (0)));
                      (*-0.125 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp1))) (F_check_ge (0) (0)));
                      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (7
                                                                    - (s IDputRGBAAseparate8bittile__x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (0) (0)))]
    | 25%positive => [(*-1 0*) F_max0_pre_decrement ((s IDputRGBAAseparate8bittile__tmp)) (1);
                      (*-0.00416667 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + (s IDputRGBAAseparate8bittile__x))) (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__x)) (0));
                      (*-0.0291667 0*) F_binom_monotonic 2 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0));
                      (*-0.00416667 0*) F_binom_monotonic 2 (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0));
                      (*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0));
                      (*-0.125 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp1))) (F_check_ge (0) (0)));
                      (*-0.0333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__x)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.0333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (7
                                                                    - (s IDputRGBAAseparate8bittile__x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.0333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp))) (F_check_ge (0) (0)));
                      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__x)))) (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__x))) (F_check_ge ((s IDputRGBAAseparate8bittile__x)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__x))) (F_check_ge (0) (0)));
                      (*-0.15 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBAAseparate8bittile__x))) (F_check_ge ((s IDputRGBAAseparate8bittile__x)) (0));
                      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDputRGBAAseparate8bittile__tmp)) (0))) (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__tmp)));
                      (*-0.025 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDputRGBAAseparate8bittile__x)));
                      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBAAseparate8bittile__x)));
                      (*-0.00416667 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDputRGBAAseparate8bittile__x)) (0))) (F_max0_ge_0 ((s IDputRGBAAseparate8bittile__x)))]
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
    | 38%positive => [(*-0.125 0*) F_max0_pre_decrement ((s IDputRGBAAseparate8bittile__x)) (8)]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | _ => []
  end.


Theorem putRGBAAseparate8bittile_ai_correct:
  forall s p' s', steps (g_start putRGBAAseparate8bittile) s (g_edges putRGBAAseparate8bittile) p' s' -> putRGBAAseparate8bittile_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem putRGBAAseparate8bittile_pot_correct:
  forall s p' s',
    steps (g_start putRGBAAseparate8bittile) s (g_edges putRGBAAseparate8bittile) p' s' ->
    (putRGBAAseparate8bittile_pot (g_start putRGBAAseparate8bittile) s >= putRGBAAseparate8bittile_pot p' s')%Q.
Proof.
  check_lp putRGBAAseparate8bittile_ai_correct putRGBAAseparate8bittile_hints.
Qed.

