Require Import pasta.Pasta.

Notation IDmain_data_z := 1%positive.
Notation IDmain_data_bits := 2%positive.
Notation IDmain_data_ch := 3%positive.
Notation IDmain_data_fi_dref_off4 := 4%positive.
Notation IDmain_data_fi_dref_off8 := 5%positive.
Notation IDmain_data_gr := 6%positive.
Notation IDmain_data_fi := 7%positive.
Notation IDmain_data_results := 8%positive.
Definition main_data : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDmain_data_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmain_data_bits (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDmain_data_gr (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmain_data_gr) s) <
             (eval (EVar IDmain_data_fi_dref_off4) s))%Z)),11%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmain_data_gr) s) >=
             (eval (EVar IDmain_data_fi_dref_off4) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDmain_data_bits None),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDmain_data_ch (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDmain_data_ch) s) <
             (eval (EVar IDmain_data_fi_dref_off8) s))%Z)),23%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDmain_data_ch)
             s) >= (eval (EVar IDmain_data_fi_dref_off8) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDmain_data_gr
             (Some (EAdd (EVar IDmain_data_gr) (ENum (1))))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDmain_data_z (Some (EAdd (ENum (1))
             (EVar IDmain_data_z)))),22%positive)::
             (22%positive,AWeaken,6%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDmain_data_bits None),25%positive)::
             (25%positive,(AAssign IDmain_data_bits None),26%positive)::
             (26%positive,(AAssign IDmain_data_bits None),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDmain_data_ch
             (Some (EAdd (EVar IDmain_data_ch) (ENum (1))))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDmain_data_z (Some (EAdd (ENum (1))
             (EVar IDmain_data_z)))),32%positive)::
             (32%positive,AWeaken,15%positive)::nil
|}.

Definition main_data_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_bits) <= 0 /\ -1 * (s IDmain_data_bits) <= 0)%Z
    | 4%positive => (-1 * (s IDmain_data_bits) <= 0 /\ 1 * (s IDmain_data_bits) <= 0 /\ 1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 5%positive => (-1 * (s IDmain_data_gr) <= 0 /\ 1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_bits) <= 0 /\ -1 * (s IDmain_data_bits) <= 0)%Z
    | 6%positive => (-1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 7%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_fi_dref_off4)+ -1 * (s IDmain_data_gr) <= 0)%Z
    | 8%positive => (1 * (s IDmain_data_fi_dref_off4)+ -1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 9%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_fi_dref_off4)+ -1 * (s IDmain_data_gr) <= 0)%Z
    | 10%positive => (1 * (s IDmain_data_fi_dref_off4)+ -1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 11%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 13%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ 1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_ch) <= 0)%Z
    | 14%positive => (-1 * (s IDmain_data_ch) <= 0 /\ 1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 15%positive => (-1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 16%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0)%Z
    | 17%positive => (-1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 18%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0)%Z
    | 19%positive => (-1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) <= 0)%Z
    | 20%positive => (-1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0)%Z
    | 21%positive => (-1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) <= 0)%Z
    | 22%positive => (-1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_ch)+ 1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_z) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 25%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 27%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_ch) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 29%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_ch) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDmain_data_ch) + 1 <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0)%Z
    | 31%positive => (-1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_z) <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_ch) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDmain_data_ch) + 1 <= 0 /\ 1 * (s IDmain_data_ch)+ -1 * (s IDmain_data_fi_dref_off8) <= 0 /\ -1 * (s IDmain_data_fi_dref_off4)+ 1 * (s IDmain_data_gr) + 1 <= 0 /\ -1 * (s IDmain_data_gr) <= 0 /\ -1 * (s IDmain_data_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition main_data_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDmain_data_fi_dref_off4))
                     + max0((s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 2%positive => ((s IDmain_data_z) + max0((s IDmain_data_fi_dref_off4))
                     + max0((s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 3%positive => ((s IDmain_data_z) + max0((s IDmain_data_fi_dref_off4))
                     + max0((s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 4%positive => ((s IDmain_data_z)
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr))
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 5%positive => ((s IDmain_data_z)
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr))
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 6%positive => ((s IDmain_data_z)
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr))
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 7%positive => ((s IDmain_data_z)
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr))
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 8%positive => ((s IDmain_data_z)
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr))
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 9%positive => ((s IDmain_data_z)
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr))
                     + max0((s IDmain_data_fi_dref_off4) - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 10%positive => ((s IDmain_data_z))%Q
    | 11%positive => ((s IDmain_data_z)
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 12%positive => ((s IDmain_data_z)
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 13%positive => ((s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                          + (s IDmain_data_fi_dref_off8))
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 14%positive => ((s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                          + (s IDmain_data_fi_dref_off8))
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8)))%Q
    | 15%positive => (-(1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 16%positive => (-(1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 17%positive => ((11 # 9) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (1 # 9) * (s IDmain_data_fi_dref_off4) * max0(-1
                                                                    + (s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr) * max0(-1
                                                            + (s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr))
                      - (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (11 # 9) * max0(-1 + (s IDmain_data_fi_dref_off4)
                                        - (s IDmain_data_gr)))%Q
    | 18%positive => ((11 # 9) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (1 # 9) * (s IDmain_data_fi_dref_off4) * max0(-1
                                                                    + (s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr) * max0(-1
                                                            + (s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr))
                      - (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (11 # 9) * max0(-1 + (s IDmain_data_fi_dref_off4)
                                        - (s IDmain_data_gr)))%Q
    | 19%positive => ((1 # 1) - (1 # 9) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (1 # 9) * (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 9) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr))
                      - (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (10 # 9) * max0((s IDmain_data_fi_dref_off4)
                                        - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off8)))%Q
    | 20%positive => ((1 # 1) - (1 # 9) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (1 # 9) * (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 9) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr))
                      - (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (10 # 9) * max0((s IDmain_data_fi_dref_off4)
                                        - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off8)))%Q
    | 21%positive => ((1 # 1) - (1 # 9) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (1 # 9) * (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 9) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr))
                      - (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (10 # 9) * max0((s IDmain_data_fi_dref_off4)
                                        - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off8)))%Q
    | 22%positive => (-(1 # 9) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (1 # 9) * (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 9) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr))
                      - (s IDmain_data_gr) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (10 # 9) * max0((s IDmain_data_fi_dref_off4)
                                        - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off8)))%Q
    | 23%positive => (-(1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      + (s IDmain_data_fi_dref_off4) * max0(-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8))
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      - max0(-1 + (s IDmain_data_fi_dref_off4)) * max0(-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 24%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 - (s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 25%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 - (s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 26%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 - (s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 27%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 - (s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 28%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 - (s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 29%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + max0(-(s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 30%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + max0(-(s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 31%positive => ((1 # 1) - (1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + max0(-(s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | 32%positive => (-(1 # 3) * (s IDmain_data_fi_dref_off4)
                      - (2 # 9) * (s IDmain_data_fi_dref_off4) * (s IDmain_data_gr)
                      - (s IDmain_data_fi_dref_off4) * max0((s IDmain_data_fi_dref_off8))
                      + (1 # 9) * (s IDmain_data_fi_dref_off4)^2
                      + (1 # 3) * (s IDmain_data_gr)
                      + (1 # 9) * (s IDmain_data_gr)^2 + (s IDmain_data_z)
                      + max0(-1 + (s IDmain_data_fi_dref_off4)) * max0((s IDmain_data_fi_dref_off8))
                      + max0(-(s IDmain_data_ch)
                             + (s IDmain_data_fi_dref_off8))
                      + (4 # 3) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))
                      + max0((s IDmain_data_fi_dref_off4)
                             - (s IDmain_data_gr)) * max0((s IDmain_data_fi_dref_off8))
                      - (1 # 9) * max0((s IDmain_data_fi_dref_off4)
                                       - (s IDmain_data_gr))^2)%Q
    | _ => (0 # 1)%Q
  end.

Definition main_data_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                            - (s IDmain_data_gr)) (-1
                                                                    + (s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDmain_data_fi_dref_off4)
                                           - (s IDmain_data_gr));
                     (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-0.111111 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0));
                      (*-0.222222 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)))]
    | 15%positive => []
    | 16%positive => [(*0 1.22222*) F_max0_pre_decrement ((s IDmain_data_fi_dref_off4)
                                                          - (s IDmain_data_gr)) (1);
                      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge (-1
                                                                    + (s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge (0) (0)));
                      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge (0) (0)));
                      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*0 1*) F_max0_monotonic (F_check_ge (-(s IDmain_data_ch)
                                                            + (s IDmain_data_fi_dref_off8)) (-1
                                                                    - (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8)));
                      (*0 0.111111*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmain_data_fi_dref_off4)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmain_data_fi_dref_off4)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDmain_data_fi_dref_off4))) (F_check_ge (-1
                                                                    + (s IDmain_data_fi_dref_off4)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)));
                      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off4)
                                                                    - (s IDmain_data_gr))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 - (s IDmain_data_ch)
                                                                 + (s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0))]
    | 23%positive => [(*0 1*) F_max0_pre_decrement (-(s IDmain_data_ch)
                                                    + (s IDmain_data_fi_dref_off8)) (1);
                      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off4))) (F_check_ge (0) (0)));
                      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))) (F_check_ge (-1
                                                                    - (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDmain_data_fi_dref_off4))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDmain_data_fi_dref_off4)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDmain_data_fi_dref_off4)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDmain_data_fi_dref_off4))) (F_check_ge (-1
                                                                    + (s IDmain_data_fi_dref_off4)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDmain_data_ch)
                                                                    + (s IDmain_data_fi_dref_off8))) (F_check_ge (0) (0)))]
    | _ => []
  end.


Theorem main_data_ai_correct:
  forall s p' s', steps (g_start main_data) s (g_edges main_data) p' s' -> main_data_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem main_data_pot_correct:
  forall s p' s',
    steps (g_start main_data) s (g_edges main_data) p' s' ->
    (main_data_pot (g_start main_data) s >= main_data_pot p' s')%Q.
Proof.
  check_lp main_data_ai_correct main_data_hints.
Qed.

