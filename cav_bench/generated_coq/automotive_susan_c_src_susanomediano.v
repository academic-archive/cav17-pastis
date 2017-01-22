Require Import pasta.Pasta.

Notation IDmedian_z := 1%positive.
Notation IDmedian__tmp := 2%positive.
Notation IDmedian__tmp1 := 3%positive.
Notation IDmedian__tmp2 := 4%positive.
Notation IDmedian_k := 5%positive.
Notation IDmedian_l := 6%positive.
Notation IDmedian_p_off0 := 7%positive.
Notation IDmedian_p_off12 := 8%positive.
Notation IDmedian_p_off16 := 9%positive.
Notation IDmedian_p_off20 := 10%positive.
Notation IDmedian_p_off24 := 11%positive.
Notation IDmedian_p_off28 := 12%positive.
Notation IDmedian_p_off4 := 13%positive.
Notation IDmedian_p_off8 := 14%positive.
Notation IDmedian_tmp := 15%positive.
Notation IDmedian_i := 16%positive.
Notation IDmedian_in := 17%positive.
Notation IDmedian_j := 18%positive.
Notation IDmedian_x_size := 19%positive.
Definition median : graph := {|
  g_start := 1%positive;
  g_end := 18%positive;
  g_edges := (1%positive,(AAssign IDmedian_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmedian__tmp2 (Some (EVar IDmedian_i))),
             3%positive)::
             (3%positive,(AAssign IDmedian__tmp1 (Some (EVar IDmedian_j))),
             4%positive)::
             (4%positive,(AAssign IDmedian__tmp
             (Some (EVar IDmedian_x_size))),5%positive)::
             (5%positive,(AAssign IDmedian_p_off0 None),6%positive)::
             (6%positive,(AAssign IDmedian_p_off4 None),7%positive)::
             (7%positive,(AAssign IDmedian_p_off8 None),8%positive)::
             (8%positive,(AAssign IDmedian_p_off12 None),9%positive)::
             (9%positive,(AAssign IDmedian_p_off16 None),10%positive)::
             (10%positive,(AAssign IDmedian_p_off20 None),11%positive)::
             (11%positive,(AAssign IDmedian_p_off24 None),12%positive)::
             (12%positive,(AAssign IDmedian_p_off28 None),13%positive)::
             (13%positive,(AAssign IDmedian_k (Some (ENum (0)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDmedian_k) s) <
             (eval (ENum (7)) s))%Z)),19%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDmedian_k) s) >=
             (eval (ENum (7)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign IDmedian_l (Some (ENum (0)))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDmedian_l) s) <
             (eval (ESub (ENum (7)) (EVar IDmedian_k)) s))%Z)),31%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDmedian_l) s) >=
             (eval (ESub (ENum (7)) (EVar IDmedian_k)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDmedian_k (Some (EAdd (EVar IDmedian_k)
             (ENum (1))))),27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDmedian_z (Some (EAdd (ENum (1))
             (EVar IDmedian_z)))),30%positive)::
             (30%positive,AWeaken,16%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (32%positive,ANone,35%positive)::
             (33%positive,(AAssign IDmedian_tmp None),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDmedian_l (Some (EAdd (EVar IDmedian_l)
             (ENum (1))))),37%positive)::(37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDmedian_z (Some (EAdd (ENum (1))
             (EVar IDmedian_z)))),40%positive)::
             (40%positive,AWeaken,23%positive)::nil
|}.

Definition median_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 4%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 6%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 8%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 10%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 12%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 14%positive => (1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 15%positive => (-1 * (s IDmedian_k) <= 0 /\ 1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_z) <= 0)%Z
    | 16%positive => (-1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 17%positive => (-1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k) + 7 <= 0)%Z
    | 18%positive => (-1 * (s IDmedian_k) + 7 <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 19%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0)%Z
    | 20%positive => (1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 21%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ 1 * (s IDmedian_l) <= 0 /\ -1 * (s IDmedian_l) <= 0)%Z
    | 22%positive => (-1 * (s IDmedian_l) <= 0 /\ 1 * (s IDmedian_l) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 23%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ -1 * (s IDmedian_k) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0)%Z
    | 24%positive => (1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0 /\ -1 * (s IDmedian_k) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 7 <= 0)%Z
    | 25%positive => (-1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 7 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_k) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0)%Z
    | 26%positive => (1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0 /\ -1 * (s IDmedian_k) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 7 <= 0)%Z
    | 27%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -8 <= 0 /\ -1 * (s IDmedian_k) + 1 <= 0 /\ 1 * (s IDmedian_k) + -7 <= 0 /\ -1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 8 <= 0)%Z
    | 28%positive => (-1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 8 <= 0 /\ 1 * (s IDmedian_k) + -7 <= 0 /\ -1 * (s IDmedian_k) + 1 <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -8 <= 0 /\ -1 * (s IDmedian_z) <= 0)%Z
    | 29%positive => (-1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -8 <= 0 /\ -1 * (s IDmedian_k) + 1 <= 0 /\ 1 * (s IDmedian_k) + -7 <= 0 /\ -1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 8 <= 0)%Z
    | 30%positive => (-1 * (s IDmedian_k)+ -1 * (s IDmedian_l) + 8 <= 0 /\ 1 * (s IDmedian_k) + -7 <= 0 /\ -1 * (s IDmedian_k) + 1 <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -8 <= 0 /\ -1 * (s IDmedian_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -6 <= 0)%Z
    | 32%positive => (1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 33%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -6 <= 0)%Z
    | 34%positive => (1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 35%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -6 <= 0)%Z
    | 36%positive => (1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -6 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k) + -6 <= 0 /\ -1 * (s IDmedian_l) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 37%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0 /\ -1 * (s IDmedian_l) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDmedian_l) + 1 <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ -1 * (s IDmedian_k) <= 0)%Z
    | 39%positive => (-1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_z) <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0 /\ -1 * (s IDmedian_l) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDmedian_l) + 1 <= 0 /\ 1 * (s IDmedian_k)+ 1 * (s IDmedian_l) + -7 <= 0 /\ -1 * (s IDmedian_k) <= 0 /\ -1 * (s IDmedian_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition median_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((50 # 1))%Q
    | 2%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 3%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 4%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 5%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 6%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 7%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 8%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 9%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 10%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 11%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 12%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 13%positive => ((50 # 1) + (s IDmedian_z))%Q
    | 14%positive => ((1 # 1) - (s IDmedian_k) + (s IDmedian_z)
                      + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(7 - (s IDmedian_k)))%Q
    | 15%positive => ((1 # 1) - (s IDmedian_k) + (s IDmedian_z)
                      + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(7 - (s IDmedian_k)))%Q
    | 16%positive => ((1 # 1) - (s IDmedian_k) + (s IDmedian_z)
                      + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(7 - (s IDmedian_k)))%Q
    | 17%positive => ((1 # 1) - (s IDmedian_k) + (s IDmedian_z)
                      + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(7 - (s IDmedian_k)))%Q
    | 18%positive => ((s IDmedian_z))%Q
    | 19%positive => ((1 # 1) - (s IDmedian_k) + (s IDmedian_z)
                      + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(7 - (s IDmedian_k)))%Q
    | 20%positive => ((8 # 1) - (s IDmedian_k) + (s IDmedian_z)
                      + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k)))%Q
    | 21%positive => ((14 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 22%positive => ((14 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 23%positive => ((14 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 24%positive => ((14 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 25%positive => ((7 # 1) - (4 # 5) * (s IDmedian_l) + (s IDmedian_z)
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      + max0(6 - (s IDmedian_k) - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 26%positive => ((7 # 1) - (4 # 5) * (s IDmedian_l) + (s IDmedian_z)
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      + max0(6 - (s IDmedian_k) - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 27%positive => ((7 # 1) - (4 # 5) * (s IDmedian_l) + (s IDmedian_z)
                      + (7 # 1) * max0(7 - (s IDmedian_k))
                      + max0(7 - (s IDmedian_k) - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 28%positive => ((7 # 1) - (4 # 5) * (s IDmedian_l) + (s IDmedian_z)
                      + (7 # 1) * max0(7 - (s IDmedian_k))
                      + max0(7 - (s IDmedian_k) - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 29%positive => ((7 # 1) - (4 # 5) * (s IDmedian_l) + (s IDmedian_z)
                      + (7 # 1) * max0(7 - (s IDmedian_k))
                      + max0(7 - (s IDmedian_k) - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 30%positive => ((6 # 1) - (4 # 5) * (s IDmedian_l) + (s IDmedian_z)
                      + (7 # 1) * max0(7 - (s IDmedian_k))
                      + max0(7 - (s IDmedian_k) - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 31%positive => ((14 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 32%positive => ((66 # 5) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(6 - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l)))%Q
    | 33%positive => ((66 # 5) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(6 - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l)))%Q
    | 34%positive => ((66 # 5) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(6 - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l)))%Q
    | 35%positive => ((66 # 5) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(6 - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l)))%Q
    | 36%positive => ((66 # 5) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(6 - (s IDmedian_l))
                      - (2 # 5) * max0(7 - (s IDmedian_l)))%Q
    | 37%positive => ((15 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 38%positive => ((15 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 39%positive => ((15 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | 40%positive => ((14 # 1) - (s IDmedian_k) - (9 # 5) * (s IDmedian_l)
                      + (s IDmedian_z) + max0(-1 + (s IDmedian_k))
                      + (7 # 1) * max0(6 - (s IDmedian_k))
                      - (2 # 5) * max0(7 - (s IDmedian_l))
                      - (2 # 5) * max0(8 - (s IDmedian_l)))%Q
    | _ => (0 # 1)%Q
  end.

Definition median_hints (p : node) (s : state) := 
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
    | 16%positive => []
    | 17%positive => [(*-7 0*) F_max0_monotonic (F_check_ge (7
                                                             - (s IDmedian_k)) (6
                                                                    - (s IDmedian_k)));
                      (*-7 0*) F_max0_ge_0 (6 - (s IDmedian_k));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDmedian_k))) (F_check_ge (-1
                                                                    + (s IDmedian_k)) (0))]
    | 18%positive => []
    | 19%positive => [(*1.82671e-12 7*) F_max0_pre_decrement (7
                                                              - (s IDmedian_k)) (1)]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*0 1*) F_max0_monotonic (F_check_ge (7
                                                            - (s IDmedian_k)
                                                            - (s IDmedian_l)) (6
                                                                    - (s IDmedian_k)
                                                                    - (s IDmedian_l)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDmedian_k)
                                                                    - (s IDmedian_l)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDmedian_k)
                                                                    - (s IDmedian_l)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDmedian_k))) (F_check_ge (0) (0))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_max0_ge_0 (7 - (s IDmedian_k)
                                            - (s IDmedian_l));
                      (*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDmedian_l)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDmedian_l)));
                      (*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDmedian_l)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDmedian_l)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmedian_k)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmedian_k)))]
    | 31%positive => [(*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDmedian_l)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDmedian_l)));
                      (*0 0.4*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                                    - 
                                                                    (s IDmedian_l))) (F_check_ge (6
                                                                    - (s IDmedian_l)) (0))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | _ => []
  end.


Theorem median_ai_correct:
  forall s p' s', steps (g_start median) s (g_edges median) p' s' -> median_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem median_pot_correct:
  forall s p' s',
    steps (g_start median) s (g_edges median) p' s' ->
    (median_pot (g_start median) s >= median_pot p' s')%Q.
Proof.
  check_lp median_ai_correct median_hints.
Qed.

