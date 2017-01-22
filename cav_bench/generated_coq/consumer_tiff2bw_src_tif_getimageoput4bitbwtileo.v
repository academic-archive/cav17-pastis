Require Import pasta.Pasta.

Notation IDput4bitbwtile_z := 1%positive.
Notation IDput4bitbwtile__tmp := 2%positive.
Notation IDput4bitbwtile__tmp1 := 3%positive.
Notation IDput4bitbwtile__tmp2 := 4%positive.
Notation IDput4bitbwtile__tmp3 := 5%positive.
Notation IDput4bitbwtile__tmp4 := 6%positive.
Notation IDput4bitbwtile__tmp5 := 7%positive.
Notation IDput4bitbwtile__x := 8%positive.
Notation IDput4bitbwtile_cp := 9%positive.
Notation IDput4bitbwtile_fromskew := 10%positive.
Notation IDput4bitbwtile_h := 11%positive.
Notation IDput4bitbwtile_img := 12%positive.
Notation IDput4bitbwtile_pp := 13%positive.
Notation IDput4bitbwtile_toskew := 14%positive.
Notation IDput4bitbwtile_w := 15%positive.
Notation IDput4bitbwtile_x := 16%positive.
Notation IDput4bitbwtile_y := 17%positive.
Definition put4bitbwtile : graph := {|
  g_start := 1%positive;
  g_end := 17%positive;
  g_edges := (1%positive,(AAssign IDput4bitbwtile_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDput4bitbwtile__x)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDput4bitbwtile__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDput4bitbwtile__tmp5
             (Some (EVar IDput4bitbwtile_x))),6%positive)::
             (6%positive,(AAssign IDput4bitbwtile__tmp4
             (Some (EVar IDput4bitbwtile_y))),7%positive)::
             (7%positive,(AAssign IDput4bitbwtile__tmp1
             (Some (EVar IDput4bitbwtile_w))),8%positive)::
             (8%positive,(AAssign IDput4bitbwtile__tmp
             (Some (EVar IDput4bitbwtile_h))),9%positive)::
             (9%positive,(AAssign IDput4bitbwtile__tmp2
             (Some (EVar IDput4bitbwtile_fromskew))),10%positive)::
             (10%positive,(AAssign IDput4bitbwtile__tmp3
             (Some (EVar IDput4bitbwtile_toskew))),11%positive)::
             (11%positive,(AAssign IDput4bitbwtile__tmp2 None),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDput4bitbwtile__tmp
             (Some (EAdd (EVar IDput4bitbwtile__tmp) (ENum (-1))))),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDput4bitbwtile__tmp) s) >
             (eval (ENum (0)) s))%Z)),18%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDput4bitbwtile__tmp) s) <=
             (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDput4bitbwtile__x
             (Some (EVar IDput4bitbwtile__tmp1))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDput4bitbwtile__x)
             s) >= (eval (ENum (2)) s))%Z)),31%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDput4bitbwtile__x)
             s) < (eval (ENum (2)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDput4bitbwtile__x)
             s) <> (eval (ENum (0)) s))%Z)),26%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDput4bitbwtile__x)
             s) = (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,28%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDput4bitbwtile_z (Some (EAdd (ENum (1))
             (EVar IDput4bitbwtile_z)))),13%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDput4bitbwtile__x
             (Some (ESub (EVar IDput4bitbwtile__x) (ENum (2))))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDput4bitbwtile_z (Some (EAdd (ENum (1))
             (EVar IDput4bitbwtile_z)))),37%positive)::
             (37%positive,AWeaken,22%positive)::nil
|}.

Definition put4bitbwtile_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 3%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 4%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDput4bitbwtile__tmp) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 6%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDput4bitbwtile__tmp) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 8%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 10%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 11%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 12%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ 1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 13%positive => (-1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 14%positive => (-1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 15%positive => (-1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 16%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile__tmp) <= 0)%Z
    | 17%positive => (1 * (s IDput4bitbwtile__tmp) <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 18%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 20%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 22%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile__x) + -1 <= 0)%Z
    | 24%positive => (1 * (s IDput4bitbwtile__x) + -1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile__x) <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 26%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile__x) + -1 <= 0)%Z
    | 27%positive => (1 * (s IDput4bitbwtile__x) + -1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile__x) + -1 <= 0)%Z
    | 29%positive => (1 * (s IDput4bitbwtile__x) + -1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ 1 * (s IDput4bitbwtile__x) + -1 <= 0)%Z
    | 31%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) + 2 <= 0)%Z
    | 32%positive => (-1 * (s IDput4bitbwtile__x) + 2 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__x) + 2 <= 0)%Z
    | 34%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 35%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) <= 0)%Z
    | 36%positive => (-1 * (s IDput4bitbwtile_z) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile__x) <= 0)%Z
    | 37%positive => (-1 * (s IDput4bitbwtile__x) <= 0 /\ -1 * (s IDput4bitbwtile__tmp) + 1 <= 0 /\ -1 * (s IDput4bitbwtile_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition put4bitbwtile_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 2%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 3%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 4%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 5%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 6%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 7%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile_w)))%Q
    | 8%positive => ((s IDput4bitbwtile_z) + max0(-1 + (s IDput4bitbwtile_h))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile_h)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 9%positive => ((s IDput4bitbwtile_z)
                     + max0(-1 + (s IDput4bitbwtile__tmp))
                     + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 10%positive => ((s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 11%positive => ((s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 12%positive => ((s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 13%positive => ((s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 14%positive => ((s IDput4bitbwtile_z) + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 15%positive => ((s IDput4bitbwtile_z) + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 16%positive => ((s IDput4bitbwtile_z) + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 17%positive => ((s IDput4bitbwtile_z))%Q
    | 18%positive => ((s IDput4bitbwtile_z) + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 19%positive => ((s IDput4bitbwtile_z) + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 20%positive => (-(1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__x))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__x))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 21%positive => (-(1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__x))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__x))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 22%positive => (-(1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__x))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__x))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 23%positive => (-(1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__x))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__x))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 24%positive => ((s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp)))%Q
    | 25%positive => ((s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp)))%Q
    | 26%positive => ((s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp)))%Q
    | 27%positive => ((1 # 1) + (s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 28%positive => ((1 # 1) + (s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 29%positive => ((1 # 1) + (s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 30%positive => ((1 # 1) + (s IDput4bitbwtile_z)
                      + max0(-1 + (s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 31%positive => (-(1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__x))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      - (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__x))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 32%positive => ((1 # 1)
                      - (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-2 + (s IDput4bitbwtile__x))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 33%positive => ((1 # 1)
                      - (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-2 + (s IDput4bitbwtile__x))
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1)))%Q
    | 34%positive => ((1 # 1)
                      - (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * max0((s IDput4bitbwtile__x)))%Q
    | 35%positive => ((1 # 1)
                      - (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * max0((s IDput4bitbwtile__x)))%Q
    | 36%positive => ((1 # 1)
                      - (1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * max0((s IDput4bitbwtile__x)))%Q
    | 37%positive => (-(1 # 2) * (s IDput4bitbwtile__tmp) * max0((s IDput4bitbwtile__tmp1))
                      + (s IDput4bitbwtile_z)
                      + (1 # 2) * max0(-1 + (s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + max0((s IDput4bitbwtile__tmp))
                      + (1 # 2) * max0((s IDput4bitbwtile__tmp)) * max0((s IDput4bitbwtile__tmp1))
                      + (1 # 2) * max0((s IDput4bitbwtile__x)))%Q
    | _ => (0 # 1)%Q
  end.

Definition put4bitbwtile_hints (p : node) (s : state) := 
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
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDput4bitbwtile__tmp)) (-1
                                                                    + (s IDput4bitbwtile__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDput4bitbwtile__tmp));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDput4bitbwtile__tmp))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDput4bitbwtile__tmp1))) (F_check_ge (0) (0)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-0.5 0*) F_max0_ge_0 ((s IDput4bitbwtile__x));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDput4bitbwtile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDput4bitbwtile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDput4bitbwtile__x))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDput4bitbwtile__tmp))) (F_check_ge ((s IDput4bitbwtile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDput4bitbwtile__tmp1))) (F_check_ge (0) (0)))]
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_pre_decrement ((s IDput4bitbwtile__tmp)) (1)]
    | 26%positive => [(*-1 0*) F_max0_pre_decrement ((s IDput4bitbwtile__tmp)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-0.5 0*) F_max0_pre_decrement ((s IDput4bitbwtile__x)) (2);
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDput4bitbwtile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDput4bitbwtile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDput4bitbwtile__x))) (F_check_ge (0) (0)))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDput4bitbwtile__tmp))) (F_check_ge (-1
                                                                    + (s IDput4bitbwtile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDput4bitbwtile__x))) (F_check_ge (0) (0)))]
    | _ => []
  end.


Theorem put4bitbwtile_ai_correct:
  forall s p' s', steps (g_start put4bitbwtile) s (g_edges put4bitbwtile) p' s' -> put4bitbwtile_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem put4bitbwtile_pot_correct:
  forall s p' s',
    steps (g_start put4bitbwtile) s (g_edges put4bitbwtile) p' s' ->
    (put4bitbwtile_pot (g_start put4bitbwtile) s >= put4bitbwtile_pot p' s')%Q.
Proof.
  check_lp put4bitbwtile_ai_correct put4bitbwtile_hints.
Qed.

