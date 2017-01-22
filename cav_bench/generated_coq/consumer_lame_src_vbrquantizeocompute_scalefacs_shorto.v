Require Import pasta.Pasta.

Notation IDcompute_scalefacs_short_z := 1%positive.
Notation IDcompute_scalefacs_short_i := 2%positive.
Notation IDcompute_scalefacs_short_ifqstep_inv := 3%positive.
Notation IDcompute_scalefacs_short_sfb := 4%positive.
Notation IDcompute_scalefacs_short_cod_info := 5%positive.
Notation IDcompute_scalefacs_short_scalefac := 6%positive.
Notation IDcompute_scalefacs_short_vbrsf := 7%positive.
Definition compute_scalefacs_short : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDcompute_scalefacs_short_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcompute_scalefacs_short_ifqstep_inv
             None),3%positive)::
             (3%positive,(AAssign IDcompute_scalefacs_short_sfb
             (Some (ENum (0)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_short_sfb) s) <
             (eval (ENum (12)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_short_sfb) s) >=
             (eval (ENum (12)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDcompute_scalefacs_short_i
             (Some (ENum (0)))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_short_i) s) <
             (eval (ENum (3)) s))%Z)),21%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_short_i) s) >=
             (eval (ENum (3)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDcompute_scalefacs_short_sfb
             (Some (EAdd (EVar IDcompute_scalefacs_short_sfb) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcompute_scalefacs_short_z
             (Some (EAdd (ENum (1)) (EVar IDcompute_scalefacs_short_z)))),
             20%positive)::(20%positive,AWeaken,6%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_short_sfb) s) <
             (eval (ENum (6)) s))%Z)),26%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_short_sfb) s) >=
             (eval (ENum (6)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,29%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,ANone,30%positive)::
             (29%positive,ANone,31%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDcompute_scalefacs_short_i
             (Some (EAdd (EVar IDcompute_scalefacs_short_i) (ENum (1))))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDcompute_scalefacs_short_z
             (Some (EAdd (ENum (1)) (EVar IDcompute_scalefacs_short_z)))),
             36%positive)::(36%positive,AWeaken,13%positive)::nil
|}.

Definition compute_scalefacs_short_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_z) <= 0)%Z
    | 4%positive => (1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 5%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 7%positive => (-1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) + 12 <= 0)%Z
    | 8%positive => (-1 * (s IDcompute_scalefacs_short_sfb) + 12 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) + -11 <= 0)%Z
    | 10%positive => (1 * (s IDcompute_scalefacs_short_sfb) + -11 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 11%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) + -11 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcompute_scalefacs_short_i) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) + -11 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 13%positive => (-1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0)%Z
    | 14%positive => (1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 3 <= 0)%Z
    | 15%positive => (-1 * (s IDcompute_scalefacs_short_i) + 3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0)%Z
    | 16%positive => (1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 3 <= 0)%Z
    | 17%positive => (-1 * (s IDcompute_scalefacs_short_i) + 3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDcompute_scalefacs_short_sfb) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 3 <= 0)%Z
    | 19%positive => (-1 * (s IDcompute_scalefacs_short_i) + 3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDcompute_scalefacs_short_sfb) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0)%Z
    | 22%positive => (1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 23%positive => (-1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) + 6 <= 0)%Z
    | 24%positive => (-1 * (s IDcompute_scalefacs_short_sfb) + 6 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0)%Z
    | 25%positive => (-1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) + 6 <= 0)%Z
    | 26%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) + -5 <= 0)%Z
    | 27%positive => (1 * (s IDcompute_scalefacs_short_sfb) + -5 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 28%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ 1 * (s IDcompute_scalefacs_short_sfb) + -5 <= 0)%Z
    | 29%positive => (1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 30%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0)%Z
    | 31%positive => (1 * (s IDcompute_scalefacs_short_i) + -2 <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0)%Z
    | 32%positive => (-1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -2 <= 0)%Z
    | 33%positive => (-1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0)%Z
    | 34%positive => (1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) <= 0)%Z
    | 35%positive => (-1 * (s IDcompute_scalefacs_short_z) <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_short_i) + -3 <= 0)%Z
    | 36%positive => (1 * (s IDcompute_scalefacs_short_i) + -3 <= 0 /\ -1 * (s IDcompute_scalefacs_short_i) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_short_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_short_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition compute_scalefacs_short_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((48 # 1))%Q
    | 2%positive => ((48 # 1) + (s IDcompute_scalefacs_short_z))%Q
    | 3%positive => ((48 # 1) + (s IDcompute_scalefacs_short_z))%Q
    | 4%positive => ((s IDcompute_scalefacs_short_z)
                     + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 5%positive => ((s IDcompute_scalefacs_short_z)
                     + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 6%positive => ((s IDcompute_scalefacs_short_z)
                     + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 7%positive => ((s IDcompute_scalefacs_short_z)
                     + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 8%positive => ((s IDcompute_scalefacs_short_z))%Q
    | 9%positive => ((s IDcompute_scalefacs_short_z)
                     + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 10%positive => ((s IDcompute_scalefacs_short_z)
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 11%positive => (-(3 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 12%positive => (-(3 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 13%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 14%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 15%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 16%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 17%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 18%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 19%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 20%positive => ((s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(12 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 21%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 22%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 23%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 24%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 25%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 26%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 27%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 28%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 29%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(2 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 30%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(2 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 31%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(2 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 32%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(2 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 33%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 34%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 35%positive => ((2 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | 36%positive => ((1 # 1) + (s IDcompute_scalefacs_short_z)
                      + max0(3 - (s IDcompute_scalefacs_short_i))
                      + (4 # 1) * max0(11 - (s IDcompute_scalefacs_short_sfb)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compute_scalefacs_short_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (12
                                                            - (s IDcompute_scalefacs_short_sfb)) (11
                                                                    - (s IDcompute_scalefacs_short_sfb)));
                     (*-4 0*) F_max0_ge_0 (11
                                           - (s IDcompute_scalefacs_short_sfb))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-4 0*) F_max0_pre_decrement (12
                                                     - (s IDcompute_scalefacs_short_sfb)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_ge_0 (3
                                            - (s IDcompute_scalefacs_short_i))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*0 1*) F_max0_pre_decrement (3
                                                    - (s IDcompute_scalefacs_short_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_pre_decrement (3
                                                     - (s IDcompute_scalefacs_short_i)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcompute_scalefacs_short_z))) (F_check_ge ((s IDcompute_scalefacs_short_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompute_scalefacs_short_z)) (0))) (F_max0_ge_0 ((s IDcompute_scalefacs_short_z)))]
    | _ => []
  end.


Theorem compute_scalefacs_short_ai_correct:
  forall s p' s', steps (g_start compute_scalefacs_short) s (g_edges compute_scalefacs_short) p' s' -> compute_scalefacs_short_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compute_scalefacs_short_pot_correct:
  forall s p' s',
    steps (g_start compute_scalefacs_short) s (g_edges compute_scalefacs_short) p' s' ->
    (compute_scalefacs_short_pot (g_start compute_scalefacs_short) s >= compute_scalefacs_short_pot p' s')%Q.
Proof.
  check_lp compute_scalefacs_short_ai_correct compute_scalefacs_short_hints.
Qed.

