Require Import pasta.Pasta.

Notation IDfill_inverse_cmap_z := 1%positive.
Notation IDfill_inverse_cmap__tmp := 2%positive.
Notation IDfill_inverse_cmap__tmp1 := 3%positive.
Notation IDfill_inverse_cmap__tmp2 := 4%positive.
Notation IDfill_inverse_cmap_ic0 := 5%positive.
Notation IDfill_inverse_cmap_ic1 := 6%positive.
Notation IDfill_inverse_cmap_ic2 := 7%positive.
Notation IDfill_inverse_cmap_minc0 := 8%positive.
Notation IDfill_inverse_cmap_minc1 := 9%positive.
Notation IDfill_inverse_cmap_minc2 := 10%positive.
Notation IDfill_inverse_cmap_numcolors := 11%positive.
Notation IDfill_inverse_cmap_c0 := 12%positive.
Notation IDfill_inverse_cmap_c1 := 13%positive.
Notation IDfill_inverse_cmap_c2 := 14%positive.
Notation IDfill_inverse_cmap_cinfo := 15%positive.
Definition fill_inverse_cmap : graph := {|
  g_start := 1%positive;
  g_end := 20%positive;
  g_edges := (1%positive,(AAssign IDfill_inverse_cmap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfill_inverse_cmap__tmp2
             (Some (EVar IDfill_inverse_cmap_c0))),3%positive)::
             (3%positive,(AAssign IDfill_inverse_cmap__tmp1
             (Some (EVar IDfill_inverse_cmap_c1))),4%positive)::
             (4%positive,(AAssign IDfill_inverse_cmap__tmp
             (Some (EVar IDfill_inverse_cmap_c2))),5%positive)::
             (5%positive,(AAssign IDfill_inverse_cmap__tmp2 None),6%positive)::
             (6%positive,(AAssign IDfill_inverse_cmap__tmp1 None),7%positive)::
             (7%positive,(AAssign IDfill_inverse_cmap__tmp None),8%positive)::
             (8%positive,(AAssign IDfill_inverse_cmap_minc0 None),9%positive)::
             (9%positive,(AAssign IDfill_inverse_cmap_minc1 None),
             10%positive)::
             (10%positive,(AAssign IDfill_inverse_cmap_minc2 None),
             11%positive)::
             (11%positive,(AAssign IDfill_inverse_cmap_numcolors None),
             12%positive)::
             (12%positive,(AAssign IDfill_inverse_cmap__tmp2 None),
             13%positive)::
             (13%positive,(AAssign IDfill_inverse_cmap__tmp1 None),
             14%positive)::
             (14%positive,(AAssign IDfill_inverse_cmap__tmp None),
             15%positive)::
             (15%positive,(AAssign IDfill_inverse_cmap_ic0
             (Some (ENum (0)))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDfill_inverse_cmap_ic0) s) <
             (eval (ENum (4)) s))%Z)),21%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDfill_inverse_cmap_ic0) s) >=
             (eval (ENum (4)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDfill_inverse_cmap_ic1
             (Some (ENum (0)))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDfill_inverse_cmap_ic1) s) <
             (eval (ENum (8)) s))%Z)),33%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDfill_inverse_cmap_ic1) s) >=
             (eval (ENum (8)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDfill_inverse_cmap_ic0
             (Some (EAdd (EVar IDfill_inverse_cmap_ic0) (ENum (1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDfill_inverse_cmap_z
             (Some (EAdd (ENum (1)) (EVar IDfill_inverse_cmap_z)))),
             32%positive)::(32%positive,AWeaken,18%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDfill_inverse_cmap_ic2
             (Some (ENum (0)))),35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDfill_inverse_cmap_ic2) s) <
             (eval (ENum (4)) s))%Z)),45%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EVar IDfill_inverse_cmap_ic2) s) >=
             (eval (ENum (4)) s))%Z)),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDfill_inverse_cmap_ic1
             (Some (EAdd (EVar IDfill_inverse_cmap_ic1) (ENum (1))))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDfill_inverse_cmap_z
             (Some (EAdd (ENum (1)) (EVar IDfill_inverse_cmap_z)))),
             44%positive)::(44%positive,AWeaken,25%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDfill_inverse_cmap_ic2
             (Some (EAdd (EVar IDfill_inverse_cmap_ic2) (ENum (1))))),
             48%positive)::(48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDfill_inverse_cmap_z
             (Some (EAdd (ENum (1)) (EVar IDfill_inverse_cmap_z)))),
             51%positive)::(51%positive,AWeaken,37%positive)::nil
|}.

Definition fill_inverse_cmap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 4%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 6%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 7%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 8%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 9%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 10%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 11%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 12%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 13%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 14%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 15%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 16%positive => (1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 17%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 18%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 19%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) + 4 <= 0)%Z
    | 20%positive => (-1 * (s IDfill_inverse_cmap_ic0) + 4 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 21%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic0) + -3 <= 0)%Z
    | 22%positive => (1 * (s IDfill_inverse_cmap_ic0) + -3 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 23%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic0) + -3 <= 0 /\ 1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0)%Z
    | 24%positive => (-1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic0) + -3 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 25%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 26%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0)%Z
    | 27%positive => (-1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 28%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0)%Z
    | 29%positive => (-1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDfill_inverse_cmap_ic0) + 1 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0)%Z
    | 31%positive => (-1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDfill_inverse_cmap_ic0) + 1 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) + 8 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic1) + -7 <= 0)%Z
    | 34%positive => (1 * (s IDfill_inverse_cmap_ic1) + -7 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 35%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic1) + -7 <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) <= 0)%Z
    | 36%positive => (-1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic1) + -7 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 37%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0)%Z
    | 38%positive => (1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0)%Z
    | 39%positive => (-1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0)%Z
    | 40%positive => (1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0)%Z
    | 41%positive => (-1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDfill_inverse_cmap_ic1) + 1 <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0)%Z
    | 43%positive => (-1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDfill_inverse_cmap_ic1) + 1 <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 4 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -3 <= 0)%Z
    | 46%positive => (1 * (s IDfill_inverse_cmap_ic2) + -3 <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0)%Z
    | 47%positive => (-1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -3 <= 0)%Z
    | 48%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 1 <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0)%Z
    | 49%positive => (1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 1 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) <= 0)%Z
    | 50%positive => (-1 * (s IDfill_inverse_cmap_z) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 1 <= 0 /\ 1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0)%Z
    | 51%positive => (1 * (s IDfill_inverse_cmap_ic2) + -4 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic2) + 1 <= 0 /\ -1 * (s IDfill_inverse_cmap_ic0) <= 0 /\ -1 * (s IDfill_inverse_cmap_ic1) <= 0 /\ -1 * (s IDfill_inverse_cmap_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition fill_inverse_cmap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((164 # 1))%Q
    | 2%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 3%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 4%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 5%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 6%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 7%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 8%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 9%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 10%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 11%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 12%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 13%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 14%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 15%positive => ((164 # 1) + (s IDfill_inverse_cmap_z))%Q
    | 16%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0)))%Q
    | 17%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0)))%Q
    | 18%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0)))%Q
    | 19%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0)))%Q
    | 20%positive => ((s IDfill_inverse_cmap_z))%Q
    | 21%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0)))%Q
    | 22%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0)))%Q
    | 23%positive => (-(40 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 24%positive => (-(40 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 25%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 26%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 27%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 28%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 29%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 30%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 31%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 32%positive => ((s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(4 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 33%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 34%positive => ((6 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 35%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 36%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 37%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 38%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 39%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 40%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 41%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 42%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 43%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 44%positive => ((1 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(8 - (s IDfill_inverse_cmap_ic1)))%Q
    | 45%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 46%positive => ((3 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(3 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 47%positive => ((3 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(3 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 48%positive => ((3 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 49%positive => ((3 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 50%positive => ((3 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | 51%positive => ((2 # 1) + (s IDfill_inverse_cmap_z)
                      + (41 # 1) * max0(3 - (s IDfill_inverse_cmap_ic0))
                      + max0(4 - (s IDfill_inverse_cmap_ic2))
                      + (5 # 1) * max0(7 - (s IDfill_inverse_cmap_ic1)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fill_inverse_cmap_hints (p : node) (s : state) := 
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
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-41 0*) F_max0_monotonic (F_check_ge (4
                                                              - (s IDfill_inverse_cmap_ic0)) (3
                                                                    - (s IDfill_inverse_cmap_ic0)));
                      (*-41 0*) F_max0_ge_0 (3 - (s IDfill_inverse_cmap_ic0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-41 0*) F_max0_pre_decrement (4
                                                      - (s IDfill_inverse_cmap_ic0)) (1)]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-5 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDfill_inverse_cmap_ic1)) (7
                                                                    - (s IDfill_inverse_cmap_ic1)));
                      (*-5 0*) F_max0_ge_0 (7 - (s IDfill_inverse_cmap_ic1))]
    | 33%positive => [(*0 5*) F_max0_pre_decrement (8
                                                    - (s IDfill_inverse_cmap_ic1)) (1)]
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
    | 44%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                 - (s IDfill_inverse_cmap_ic2))) (F_check_ge (0) (0))]
    | 45%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDfill_inverse_cmap_ic2)) (1)]
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | _ => []
  end.


Theorem fill_inverse_cmap_ai_correct:
  forall s p' s', steps (g_start fill_inverse_cmap) s (g_edges fill_inverse_cmap) p' s' -> fill_inverse_cmap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fill_inverse_cmap_pot_correct:
  forall s p' s',
    steps (g_start fill_inverse_cmap) s (g_edges fill_inverse_cmap) p' s' ->
    (fill_inverse_cmap_pot (g_start fill_inverse_cmap) s >= fill_inverse_cmap_pot p' s')%Q.
Proof.
  check_lp fill_inverse_cmap_ai_correct fill_inverse_cmap_hints.
Qed.

