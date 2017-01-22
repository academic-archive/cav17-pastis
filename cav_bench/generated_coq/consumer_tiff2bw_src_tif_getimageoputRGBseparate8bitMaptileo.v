Require Import pasta.Pasta.

Notation IDputRGBseparate8bitMaptile_z := 1%positive.
Notation IDputRGBseparate8bitMaptile__tmp := 2%positive.
Notation IDputRGBseparate8bitMaptile__tmp1 := 3%positive.
Notation IDputRGBseparate8bitMaptile__tmp2 := 4%positive.
Notation IDputRGBseparate8bitMaptile__tmp3 := 5%positive.
Notation IDputRGBseparate8bitMaptile__tmp4 := 6%positive.
Notation IDputRGBseparate8bitMaptile__tmp5 := 7%positive.
Notation IDputRGBseparate8bitMaptile_a := 8%positive.
Notation IDputRGBseparate8bitMaptile_b := 9%positive.
Notation IDputRGBseparate8bitMaptile_cp := 10%positive.
Notation IDputRGBseparate8bitMaptile_fromskew := 11%positive.
Notation IDputRGBseparate8bitMaptile_g := 12%positive.
Notation IDputRGBseparate8bitMaptile_h := 13%positive.
Notation IDputRGBseparate8bitMaptile_img := 14%positive.
Notation IDputRGBseparate8bitMaptile_r := 15%positive.
Notation IDputRGBseparate8bitMaptile_toskew := 16%positive.
Notation IDputRGBseparate8bitMaptile_w := 17%positive.
Notation IDputRGBseparate8bitMaptile_x := 18%positive.
Notation IDputRGBseparate8bitMaptile_y := 19%positive.
Definition putRGBseparate8bitMaptile : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDputRGBseparate8bitMaptile_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBseparate8bitMaptile__tmp1) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBseparate8bitMaptile__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDputRGBseparate8bitMaptile__tmp1
             (Some (EVar IDputRGBseparate8bitMaptile_x))),6%positive)::
             (6%positive,(AAssign IDputRGBseparate8bitMaptile__tmp5
             (Some (EVar IDputRGBseparate8bitMaptile_y))),7%positive)::
             (7%positive,(AAssign IDputRGBseparate8bitMaptile__tmp2
             (Some (EVar IDputRGBseparate8bitMaptile_w))),8%positive)::
             (8%positive,(AAssign IDputRGBseparate8bitMaptile__tmp
             (Some (EVar IDputRGBseparate8bitMaptile_h))),9%positive)::
             (9%positive,(AAssign IDputRGBseparate8bitMaptile__tmp4
             (Some (EVar IDputRGBseparate8bitMaptile_fromskew))),10%positive)::
             (10%positive,(AAssign IDputRGBseparate8bitMaptile__tmp3
             (Some (EVar IDputRGBseparate8bitMaptile_toskew))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDputRGBseparate8bitMaptile__tmp
             (Some (EAdd (EVar IDputRGBseparate8bitMaptile__tmp)
             (ENum (-1))))),13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBseparate8bitMaptile__tmp) s) >
             (eval (ENum (0)) s))%Z)),17%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBseparate8bitMaptile__tmp) s) <=
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDputRGBseparate8bitMaptile__tmp1
             (Some (EVar IDputRGBseparate8bitMaptile__tmp2))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBseparate8bitMaptile__tmp1) s) >
             (eval (ENum (0)) s))%Z)),26%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDputRGBseparate8bitMaptile__tmp1) s) <=
             (eval (ENum (0)) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDputRGBseparate8bitMaptile_z
             (Some (EAdd (ENum (1)) (EVar IDputRGBseparate8bitMaptile_z)))),
             12%positive)::(26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDputRGBseparate8bitMaptile__tmp1
             (Some (EAdd (EVar IDputRGBseparate8bitMaptile__tmp1)
             (ENum (-1))))),29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDputRGBseparate8bitMaptile_z
             (Some (EAdd (ENum (1)) (EVar IDputRGBseparate8bitMaptile_z)))),
             32%positive)::(32%positive,AWeaken,21%positive)::nil
|}.

Definition putRGBseparate8bitMaptile_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 3%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0)%Z
    | 4%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0)%Z
    | 6%positive => (1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 8%positive => (1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 10%positive => (1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 11%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 12%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 13%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 14%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 15%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0)%Z
    | 16%positive => (1 * (s IDputRGBseparate8bitMaptile__tmp) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 17%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 19%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 21%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0)%Z
    | 23%positive => (1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ 1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0)%Z
    | 25%positive => (1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp1) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp1) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp1) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0)%Z
    | 30%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) <= 0)%Z
    | 31%positive => (-1 * (s IDputRGBseparate8bitMaptile_z) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0)%Z
    | 32%positive => (-1 * (s IDputRGBseparate8bitMaptile__tmp1) <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile__tmp) + 1 <= 0 /\ -1 * (s IDputRGBseparate8bitMaptile_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition putRGBseparate8bitMaptile_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 2%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 3%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 4%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 5%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 6%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 7%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile_w)))%Q
    | 8%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile_h)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 9%positive => ((s IDputRGBseparate8bitMaptile_z)
                     + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                     + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 10%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 11%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 12%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 13%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 14%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 15%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 16%positive => ((s IDputRGBseparate8bitMaptile_z))%Q
    | 17%positive => ((s IDputRGBseparate8bitMaptile_z)
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 18%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + (1 # 2) * max0((s IDputRGBseparate8bitMaptile__tmp))^2)%Q
    | 19%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + (1 # 2) * max0((s IDputRGBseparate8bitMaptile__tmp))^2)%Q
    | 20%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + (1 # 2) * max0((s IDputRGBseparate8bitMaptile__tmp))^2)%Q
    | 21%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      + (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 22%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      + (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 23%positive => ((1 # 1) + (s IDputRGBseparate8bitMaptile_z)
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 24%positive => ((1 # 1) + (s IDputRGBseparate8bitMaptile_z)
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 25%positive => ((1 # 1) + (s IDputRGBseparate8bitMaptile_z)
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 26%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      + (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp1))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 27%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      - (s IDputRGBseparate8bitMaptile__tmp1) * max0(-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp1) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 28%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      - (s IDputRGBseparate8bitMaptile__tmp1) * max0(-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp1) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile_z)
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 29%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      - (s IDputRGBseparate8bitMaptile__tmp1) * max0(-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp1) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile_z)
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 30%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      - (s IDputRGBseparate8bitMaptile__tmp1) * max0(-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp1) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile_z)
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 31%positive => ((3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      - (s IDputRGBseparate8bitMaptile__tmp1) * max0(-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp1) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile_z)
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | 32%positive => (-(1 # 1)
                      + (3 # 2) * (s IDputRGBseparate8bitMaptile__tmp)
                      - (s IDputRGBseparate8bitMaptile__tmp) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      - (1 # 2) * (s IDputRGBseparate8bitMaptile__tmp)^2
                      - (s IDputRGBseparate8bitMaptile__tmp1) * max0(-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile__tmp1) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + (s IDputRGBseparate8bitMaptile_z)
                      - max0(-1 + (s IDputRGBseparate8bitMaptile__tmp))
                      + (1 # 2) * max0(-1
                                       + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0(-1 + (s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2))
                      + max0((s IDputRGBseparate8bitMaptile__tmp))
                      + max0((s IDputRGBseparate8bitMaptile__tmp)) * max0((s IDputRGBseparate8bitMaptile__tmp2)))%Q
    | _ => (0 # 1)%Q
  end.

Definition putRGBseparate8bitMaptile_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDputRGBseparate8bitMaptile__tmp));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp2))) (F_check_ge (0) (0)))]
    | 16%positive => []
    | 17%positive => [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0));
                      (*0 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)))]
    | 21%positive => []
    | 22%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp1)) (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp1)));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + 
                                                                    (s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)) (0));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp1))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp2))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDputRGBseparate8bitMaptile__tmp1))) (F_check_ge (0) (0))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp1))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp1)) (0))) (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp1)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp1))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp1)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_max0_pre_decrement ((s IDputRGBseparate8bitMaptile__tmp)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp1))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp1)) (0))) (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp1)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDputRGBseparate8bitMaptile__tmp1))) (F_check_ge ((s IDputRGBseparate8bitMaptile__tmp1)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDputRGBseparate8bitMaptile__tmp))) (F_check_ge (0) (0)))]
    | _ => []
  end.


Theorem putRGBseparate8bitMaptile_ai_correct:
  forall s p' s', steps (g_start putRGBseparate8bitMaptile) s (g_edges putRGBseparate8bitMaptile) p' s' -> putRGBseparate8bitMaptile_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem putRGBseparate8bitMaptile_pot_correct:
  forall s p' s',
    steps (g_start putRGBseparate8bitMaptile) s (g_edges putRGBseparate8bitMaptile) p' s' ->
    (putRGBseparate8bitMaptile_pot (g_start putRGBseparate8bitMaptile) s >= putRGBseparate8bitMaptile_pot p' s')%Q.
Proof.
  check_lp putRGBseparate8bitMaptile_ai_correct putRGBseparate8bitMaptile_hints.
Qed.

