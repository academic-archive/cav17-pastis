Require Import pasta.Pasta.

Notation IDloop_break_z := 1%positive.
Notation IDloop_break__tmp := 2%positive.
Notation IDloop_break_cod_info_dref_off80 := 3%positive.
Notation IDloop_break_cod_info_dref_off84 := 4%positive.
Notation IDloop_break_i := 5%positive.
Notation IDloop_break_sfb := 6%positive.
Notation IDloop_break_cod_info := 7%positive.
Notation IDloop_break_scalefac := 8%positive.
Definition loop_break : graph := {|
  g_start := 1%positive;
  g_end := 52%positive;
  g_edges := (1%positive,(AAssign IDloop_break_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDloop_break_sfb)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDloop_break_cod_info_dref_off80) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDloop_break_sfb (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDloop_break_sfb)
             s) < (eval (EVar IDloop_break_cod_info_dref_off80) s))%Z)),
             41%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDloop_break_sfb)
             s) >= (eval (EVar IDloop_break_cod_info_dref_off80) s))%Z)),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDloop_break_sfb
             (Some (EVar IDloop_break_cod_info_dref_off84))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDloop_break_sfb)
             s) < (eval (ENum (12)) s))%Z)),18%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDloop_break_sfb)
             s) >= (eval (ENum (12)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDloop_break__tmp (Some (ENum (1)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,52%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDloop_break_i (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDloop_break_i) s) <
             (eval (ENum (3)) s))%Z)),30%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDloop_break_i)
             s) >= (eval (ENum (3)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDloop_break_sfb
             (Some (EAdd (EVar IDloop_break_sfb) (ENum (1))))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDloop_break_z (Some (EAdd (ENum (1))
             (EVar IDloop_break_z)))),29%positive)::
             (29%positive,AWeaken,13%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,38%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDloop_break_i
             (Some (EAdd (EVar IDloop_break_i) (ENum (1))))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDloop_break_z (Some (EAdd (ENum (1))
             (EVar IDloop_break_z)))),37%positive)::
             (37%positive,AWeaken,22%positive)::
             (38%positive,(AAssign IDloop_break__tmp (Some (ENum (0)))),
             39%positive)::(39%positive,ANone,40%positive)::
             (40%positive,AWeaken,52%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,49%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDloop_break_sfb
             (Some (EAdd (EVar IDloop_break_sfb) (ENum (1))))),45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDloop_break_z (Some (EAdd (ENum (1))
             (EVar IDloop_break_z)))),48%positive)::
             (48%positive,AWeaken,8%positive)::
             (49%positive,(AAssign IDloop_break__tmp (Some (ENum (0)))),
             50%positive)::(50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::nil
|}.

Definition loop_break_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_z) <= 0)%Z
    | 3%positive => (-1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0)%Z
    | 4%positive => (-1 * (s IDloop_break_sfb) <= 0 /\ 1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80) <= 0)%Z
    | 5%positive => (-1 * (s IDloop_break_cod_info_dref_off80) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0)%Z
    | 6%positive => (1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80) <= 0 /\ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0)%Z
    | 7%positive => (-1 * (s IDloop_break_sfb) <= 0 /\ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_z) <= 0)%Z
    | 8%positive => (-1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0)%Z
    | 9%positive => (-1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_cod_info_dref_off80)+ -1 * (s IDloop_break_sfb) <= 0)%Z
    | 10%positive => (1 * (s IDloop_break_cod_info_dref_off80)+ -1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0)%Z
    | 11%positive => (-1 * (s IDloop_break_z) <= 0)%Z
    | 12%positive => (-1 * (s IDloop_break_z) <= 0)%Z
    | 13%positive => (-1 * (s IDloop_break_z) <= 0)%Z
    | 14%positive => (-1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) + 12 <= 0)%Z
    | 15%positive => (-1 * (s IDloop_break_sfb) + 12 <= 0 /\ -1 * (s IDloop_break_z) <= 0)%Z
    | 16%positive => (-1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) + 12 <= 0 /\ 1 * (s IDloop_break__tmp) + -1 <= 0 /\ -1 * (s IDloop_break__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDloop_break__tmp) + 1 <= 0 /\ 1 * (s IDloop_break__tmp) + -1 <= 0 /\ -1 * (s IDloop_break_sfb) + 12 <= 0 /\ -1 * (s IDloop_break_z) <= 0)%Z
    | 18%positive => (-1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0)%Z
    | 19%positive => (1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) <= 0)%Z
    | 20%positive => (-1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ 1 * (s IDloop_break_i) <= 0 /\ -1 * (s IDloop_break_i) <= 0)%Z
    | 21%positive => (-1 * (s IDloop_break_i) <= 0 /\ 1 * (s IDloop_break_i) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) <= 0)%Z
    | 22%positive => (-1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0)%Z
    | 23%positive => (1 * (s IDloop_break_i) + -3 <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) + 3 <= 0)%Z
    | 24%positive => (-1 * (s IDloop_break_i) + 3 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0)%Z
    | 25%positive => (1 * (s IDloop_break_i) + -3 <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) + 3 <= 0)%Z
    | 26%positive => (-1 * (s IDloop_break_i) + 3 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ 1 * (s IDloop_break_sfb) + -12 <= 0)%Z
    | 27%positive => (1 * (s IDloop_break_sfb) + -12 <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) + 3 <= 0)%Z
    | 28%positive => (-1 * (s IDloop_break_i) + 3 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ 1 * (s IDloop_break_sfb) + -12 <= 0)%Z
    | 29%positive => (1 * (s IDloop_break_sfb) + -12 <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ -1 * (s IDloop_break_i) + 3 <= 0 /\ -1 * (s IDloop_break_z) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -2 <= 0)%Z
    | 31%positive => (1 * (s IDloop_break_i) + -2 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0)%Z
    | 32%positive => (1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -2 <= 0)%Z
    | 33%positive => (1 * (s IDloop_break_i) + -2 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0)%Z
    | 34%positive => (1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ -1 * (s IDloop_break_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDloop_break_i) + 1 <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0)%Z
    | 36%positive => (1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ -1 * (s IDloop_break_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDloop_break_i) + 1 <= 0 /\ 1 * (s IDloop_break_i) + -3 <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_z) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -2 <= 0)%Z
    | 39%positive => (1 * (s IDloop_break_i) + -2 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ 1 * (s IDloop_break__tmp) <= 0 /\ -1 * (s IDloop_break__tmp) <= 0)%Z
    | 40%positive => (-1 * (s IDloop_break__tmp) <= 0 /\ 1 * (s IDloop_break__tmp) <= 0 /\ 1 * (s IDloop_break_sfb) + -11 <= 0 /\ -1 * (s IDloop_break_i) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ 1 * (s IDloop_break_i) + -2 <= 0)%Z
    | 41%positive => (-1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0)%Z
    | 43%positive => (-1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0)%Z
    | 45%positive => (-1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_sfb) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDloop_break_sfb) + 1 <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0)%Z
    | 47%positive => (-1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_sfb) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDloop_break_sfb) + 1 <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0 /\ 1 * (s IDloop_break__tmp) <= 0 /\ -1 * (s IDloop_break__tmp) <= 0)%Z
    | 51%positive => (-1 * (s IDloop_break__tmp) <= 0 /\ 1 * (s IDloop_break__tmp) <= 0 /\ -1 * (s IDloop_break_sfb) <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break_cod_info_dref_off80)+ 1 * (s IDloop_break_sfb) + 1 <= 0)%Z
    | 52%positive => (1 * (s IDloop_break__tmp) + -1 <= 0 /\ -1 * (s IDloop_break_z) <= 0 /\ -1 * (s IDloop_break__tmp) <= 0)%Z
    | _ => False
  end.

Definition loop_break_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1) * max0(12 - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)))%Q
    | 2%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)))%Q
    | 3%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)))%Q
    | 4%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)))%Q
    | 5%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)))%Q
    | 6%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)
                            - (s IDloop_break_sfb)))%Q
    | 7%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)
                            - (s IDloop_break_sfb)))%Q
    | 8%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)
                            - (s IDloop_break_sfb)))%Q
    | 9%positive => ((s IDloop_break_z)
                     + (4 # 1) * max0(12
                                      - (s IDloop_break_cod_info_dref_off84))
                     + max0((s IDloop_break_cod_info_dref_off80)
                            - (s IDloop_break_sfb)))%Q
    | 10%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 11%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 12%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 13%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 14%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 15%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 16%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 17%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 18%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 19%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 20%positive => (-(3 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 21%positive => (-(3 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 22%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 23%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 24%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 25%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 26%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 27%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 28%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 29%positive => ((s IDloop_break_z) + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(12 - (s IDloop_break_sfb)))%Q
    | 30%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 31%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(2 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 32%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(2 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 33%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(2 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 34%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 35%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 36%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 37%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(3 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 38%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(2 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 39%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(2 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 40%positive => ((2 # 1) + (s IDloop_break_z)
                      + max0(2 - (s IDloop_break_i))
                      + (4 # 1) * max0(11 - (s IDloop_break_sfb)))%Q
    | 41%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84))
                      + max0((s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb)))%Q
    | 42%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(-1 + (s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb))
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 43%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(-1 + (s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb))
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 44%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(-1 + (s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb))
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 45%positive => ((1 # 1) + (s IDloop_break_z)
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84))
                      + max0((s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb)))%Q
    | 46%positive => ((1 # 1) + (s IDloop_break_z)
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84))
                      + max0((s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb)))%Q
    | 47%positive => ((1 # 1) + (s IDloop_break_z)
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84))
                      + max0((s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb)))%Q
    | 48%positive => ((s IDloop_break_z)
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84))
                      + max0((s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb)))%Q
    | 49%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(-1 + (s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb))
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 50%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(-1 + (s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb))
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 51%positive => ((1 # 1) + (s IDloop_break_z)
                      + max0(-1 + (s IDloop_break_cod_info_dref_off80)
                             - (s IDloop_break_sfb))
                      + (4 # 1) * max0(12
                                       - (s IDloop_break_cod_info_dref_off84)))%Q
    | 52%positive => ((s IDloop_break_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition loop_break_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDloop_break_cod_info_dref_off80)
                                                            - (s IDloop_break_sfb)) (-1
                                                                    + (s IDloop_break_cod_info_dref_off80)
                                                                    - (s IDloop_break_sfb)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           + (s IDloop_break_cod_info_dref_off80)
                                           - (s IDloop_break_sfb))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (12
                                                             - (s IDloop_break_sfb)) (11
                                                                    - (s IDloop_break_sfb)));
                      (*-4 0*) F_max0_ge_0 (11 - (s IDloop_break_sfb))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-4 0*) F_max0_pre_decrement (12
                                                     - (s IDloop_break_sfb)) (1)]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                 - (s IDloop_break_i))) (F_check_ge (0) (0))]
    | 30%positive => [(*0 1*) F_max0_pre_decrement (3 - (s IDloop_break_i)) (1)]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-2 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (2 - (s IDloop_break_i));
                      (*-4 0*) F_max0_ge_0 (11 - (s IDloop_break_sfb))]
    | 41%positive => [(*-1 0*) F_max0_pre_decrement ((s IDloop_break_cod_info_dref_off80)
                                                     - (s IDloop_break_sfb)) (1)]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDloop_break_cod_info_dref_off80)
                                            - (s IDloop_break_sfb));
                      (*-4 0*) F_max0_ge_0 (12
                                            - (s IDloop_break_cod_info_dref_off84))]
    | 52%positive => []
    | _ => []
  end.


Theorem loop_break_ai_correct:
  forall s p' s', steps (g_start loop_break) s (g_edges loop_break) p' s' -> loop_break_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem loop_break_pot_correct:
  forall s p' s',
    steps (g_start loop_break) s (g_edges loop_break) p' s' ->
    (loop_break_pot (g_start loop_break) s >= loop_break_pot p' s')%Q.
Proof.
  check_lp loop_break_ai_correct loop_break_hints.
Qed.

