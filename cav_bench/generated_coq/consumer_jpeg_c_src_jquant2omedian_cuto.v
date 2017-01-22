Require Import pasta.Pasta.

Notation IDmedian_cut_z := 1%positive.
Notation IDmedian_cut__tmp := 2%positive.
Notation IDmedian_cut__tmp1 := 3%positive.
Notation IDmedian_cut_c0 := 4%positive.
Notation IDmedian_cut_c1 := 5%positive.
Notation IDmedian_cut_c2 := 6%positive.
Notation IDmedian_cut_cmax := 7%positive.
Notation IDmedian_cut_lb := 8%positive.
Notation IDmedian_cut_n := 9%positive.
Notation IDmedian_cut_boxlist := 10%positive.
Notation IDmedian_cut_cinfo := 11%positive.
Notation IDmedian_cut_desired_colors := 12%positive.
Notation IDmedian_cut_numboxes := 13%positive.
Definition median_cut : graph := {|
  g_start := 1%positive;
  g_end := 50%positive;
  g_edges := (1%positive,(AAssign IDmedian_cut_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmedian_cut__tmp
             (Some (EVar IDmedian_cut_numboxes))),3%positive)::
             (3%positive,(AAssign IDmedian_cut__tmp1
             (Some (EVar IDmedian_cut_desired_colors))),4%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmedian_cut__tmp)
             s) < (eval (EVar IDmedian_cut__tmp1) s))%Z)),8%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmedian_cut__tmp)
             s) >= (eval (EVar IDmedian_cut__tmp1) s))%Z)),7%positive)::
             (7%positive,AWeaken,50%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EMul (EVar IDmedian_cut__tmp) (ENum (2)))
             s) <= (eval (EVar IDmedian_cut__tmp1) s))%Z)),13%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EMul (EVar IDmedian_cut__tmp) (ENum (2))) s) >
             (eval (EVar IDmedian_cut__tmp1) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,16%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,48%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDmedian_cut_c0 None),18%positive)::
             (18%positive,(AAssign IDmedian_cut_c1 None),19%positive)::
             (19%positive,(AAssign IDmedian_cut_c2 None),20%positive)::
             (20%positive,(AAssign IDmedian_cut_cmax
             (Some (EVar IDmedian_cut_c1))),21%positive)::
             (21%positive,(AAssign IDmedian_cut_n (Some (ENum (1)))),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDmedian_cut_c0)
             s) > (eval (EVar IDmedian_cut_cmax) s))%Z)),25%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDmedian_cut_c0)
             s) <= (eval (EVar IDmedian_cut_cmax) s))%Z)),24%positive)::
             (24%positive,AWeaken,30%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDmedian_cut_cmax
             (Some (EVar IDmedian_cut_c0))),27%positive)::
             (27%positive,(AAssign IDmedian_cut_n (Some (ENum (0)))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDmedian_cut_c2)
             s) > (eval (EVar IDmedian_cut_cmax) s))%Z)),32%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDmedian_cut_c2)
             s) <= (eval (EVar IDmedian_cut_cmax) s))%Z)),31%positive)::
             (31%positive,AWeaken,36%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDmedian_cut_n (Some (ENum (2)))),
             34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,43%positive)::
             (36%positive,ANone,41%positive)::
             (36%positive,ANone,39%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDmedian_cut_lb None),38%positive)::
             (38%positive,ANone,43%positive)::
             (39%positive,(AAssign IDmedian_cut_lb None),40%positive)::
             (40%positive,ANone,43%positive)::
             (41%positive,(AAssign IDmedian_cut_lb None),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDmedian_cut__tmp
             (Some (EAdd (EVar IDmedian_cut__tmp) (ENum (1))))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDmedian_cut_z (Some (EAdd (ENum (1))
             (EVar IDmedian_cut_z)))),47%positive)::
             (47%positive,AWeaken,6%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::nil
|}.

Definition median_cut_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmedian_cut_z) <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_z) <= 0)%Z
    | 4%positive => (1 * (s IDmedian_cut_z) <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_z) <= 0)%Z
    | 6%positive => (-1 * (s IDmedian_cut_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ -1 * (s IDmedian_cut__tmp)+ 1 * (s IDmedian_cut__tmp1) <= 0)%Z
    | 8%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 9%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 10%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -2 * (s IDmedian_cut__tmp)+ 1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 11%positive => (-2 * (s IDmedian_cut__tmp)+ 1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 12%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -2 * (s IDmedian_cut__tmp)+ 1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ 2 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0)%Z
    | 14%positive => (2 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 15%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ 2 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0)%Z
    | 16%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 17%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 19%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 21%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_n) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDmedian_cut_n) + 1 <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_n) + 1 <= 0 /\ 1 * (s IDmedian_cut_c0)+ -1 * (s IDmedian_cut_cmax) <= 0)%Z
    | 25%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_n) + 1 <= 0 /\ -1 * (s IDmedian_cut_c0)+ 1 * (s IDmedian_cut_cmax) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDmedian_cut_c0)+ 1 * (s IDmedian_cut_cmax) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) + 1 <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 27%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_n) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ 1 * (s IDmedian_cut_n) <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 29%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 30%positive => (1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 31%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ 1 * (s IDmedian_cut_c2)+ -1 * (s IDmedian_cut_cmax) <= 0)%Z
    | 32%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_c2)+ 1 * (s IDmedian_cut_cmax) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDmedian_cut_c2)+ 1 * (s IDmedian_cut_cmax) + 1 <= 0 /\ 1 * (s IDmedian_cut_n) + -1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 34%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ -1 * (s IDmedian_cut_c2)+ 1 * (s IDmedian_cut_cmax) + 1 <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_n) + 2 <= 0)%Z
    | 35%positive => (-1 * (s IDmedian_cut_n) + 2 <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_c2)+ 1 * (s IDmedian_cut_cmax) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0)%Z
    | 37%positive => (1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 38%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0)%Z
    | 39%positive => (1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 40%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0)%Z
    | 41%positive => (1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 42%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0)%Z
    | 43%positive => (1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 44%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0)%Z
    | 45%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ -1 * (s IDmedian_cut_n) <= 0)%Z
    | 46%positive => (-1 * (s IDmedian_cut_n) <= 0 /\ -1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0)%Z
    | 47%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) <= 0 /\ 1 * (s IDmedian_cut_n) + -2 <= 0 /\ -1 * (s IDmedian_cut_n) <= 0 /\ -1 * (s IDmedian_cut_z) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDmedian_cut_z) <= 0 /\ 1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDmedian_cut__tmp)+ -1 * (s IDmedian_cut__tmp1) + 1 <= 0 /\ -1 * (s IDmedian_cut_z) <= 0)%Z
    | 50%positive => (-1 * (s IDmedian_cut_z) <= 0)%Z
    | _ => False
  end.

Definition median_cut_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDmedian_cut_desired_colors)
                          - (s IDmedian_cut_numboxes)))%Q
    | 2%positive => ((s IDmedian_cut_z)
                     + max0((s IDmedian_cut_desired_colors)
                            - (s IDmedian_cut_numboxes)))%Q
    | 3%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp)
                            + (s IDmedian_cut_desired_colors)))%Q
    | 4%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 5%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 6%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 7%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 8%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 9%positive => ((s IDmedian_cut_z)
                     + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 10%positive => ((s IDmedian_cut_z)
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 11%positive => ((s IDmedian_cut_z)
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 12%positive => ((s IDmedian_cut_z)
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 13%positive => ((s IDmedian_cut_z)
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 14%positive => ((s IDmedian_cut_z)
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 15%positive => ((s IDmedian_cut_z)
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 16%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 17%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 18%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 19%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 20%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 21%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 22%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 23%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 24%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 25%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 26%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 27%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 28%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 29%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 30%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 31%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 32%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 33%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 34%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 35%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 36%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 37%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 38%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 39%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 40%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 41%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 42%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 43%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1))
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n)))%Q
    | 44%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n))
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 45%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n))
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 46%positive => ((1 # 2) * (s IDmedian_cut_n) + (s IDmedian_cut_z)
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n))
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 47%positive => (-(1 # 1) + (1 # 2) * (s IDmedian_cut_n)
                      + (s IDmedian_cut_z)
                      + (1 # 2) * max0(2 - (s IDmedian_cut_n))
                      + max0(-(s IDmedian_cut__tmp) + (s IDmedian_cut__tmp1)))%Q
    | 48%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 49%positive => ((1 # 1) + (s IDmedian_cut_z)
                      + max0(-1 - (s IDmedian_cut__tmp)
                             + (s IDmedian_cut__tmp1)))%Q
    | 50%positive => ((s IDmedian_cut_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition median_cut_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDmedian_cut__tmp)
                                                            + (s IDmedian_cut__tmp1)) (-1
                                                                    - (s IDmedian_cut__tmp)
                                                                    + (s IDmedian_cut__tmp1)));
                     (*-1 0*) F_max0_ge_0 (-1 - (s IDmedian_cut__tmp)
                                           + (s IDmedian_cut__tmp1))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDmedian_cut__tmp)
                                                     + (s IDmedian_cut__tmp1)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDmedian_cut__tmp)
                                                     + (s IDmedian_cut__tmp1)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDmedian_cut_n)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDmedian_cut_n)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                    - 
                                                                    (s IDmedian_cut_n))) (F_check_ge (2
                                                                    - (s IDmedian_cut_n)) (0))]
    | 33%positive => []
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
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                    - 
                                                                    (s IDmedian_cut_n))) (F_check_ge (2
                                                                    - (s IDmedian_cut_n)) (0))]
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDmedian_cut__tmp)
                                            + (s IDmedian_cut__tmp1))]
    | 50%positive => []
    | _ => []
  end.


Theorem median_cut_ai_correct:
  forall s p' s', steps (g_start median_cut) s (g_edges median_cut) p' s' -> median_cut_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem median_cut_pot_correct:
  forall s p' s',
    steps (g_start median_cut) s (g_edges median_cut) p' s' ->
    (median_cut_pot (g_start median_cut) s >= median_cut_pot p' s')%Q.
Proof.
  check_lp median_cut_ai_correct median_cut_hints.
Qed.

