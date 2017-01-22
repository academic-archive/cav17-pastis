Require Import pasta.Pasta.

Notation IDset_file_type_z := 1%positive.
Notation IDset_file_type_ascii_freq := 2%positive.
Notation IDset_file_type_bin_freq := 3%positive.
Notation IDset_file_type_file_type_dref := 4%positive.
Notation IDset_file_type_n := 5%positive.
Definition set_file_type : graph := {|
  g_start := 1%positive;
  g_end := 19%positive;
  g_edges := (1%positive,(AAssign IDset_file_type_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDset_file_type_n (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDset_file_type_ascii_freq
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDset_file_type_bin_freq
             (Some (ENum (0)))),5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDset_file_type_n)
             s) < (eval (ENum (7)) s))%Z)),34%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDset_file_type_n)
             s) >= (eval (ENum (7)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDset_file_type_n)
             s) < (eval (ENum (128)) s))%Z)),27%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDset_file_type_n)
             s) >= (eval (ENum (128)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDset_file_type_n)
             s) < (eval (ENum (256)) s))%Z)),20%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDset_file_type_n)
             s) >= (eval (ENum (256)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDset_file_type_file_type_dref None),
             18%positive)::(18%positive,AWeaken,19%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDset_file_type_n
             (Some (EAdd (EVar IDset_file_type_n) (ENum (1))))),22%positive)::
             (22%positive,(AAssign IDset_file_type_bin_freq None),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDset_file_type_z (Some (EAdd (ENum (1))
             (EVar IDset_file_type_z)))),26%positive)::
             (26%positive,AWeaken,15%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDset_file_type_n
             (Some (EAdd (EVar IDset_file_type_n) (ENum (1))))),29%positive)::
             (29%positive,(AAssign IDset_file_type_ascii_freq None),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDset_file_type_z (Some (EAdd (ENum (1))
             (EVar IDset_file_type_z)))),33%positive)::
             (33%positive,AWeaken,11%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AAssign IDset_file_type_n
             (Some (EAdd (EVar IDset_file_type_n) (ENum (1))))),36%positive)::
             (36%positive,(AAssign IDset_file_type_bin_freq None),
             37%positive)::(37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDset_file_type_z (Some (EAdd (ENum (1))
             (EVar IDset_file_type_z)))),40%positive)::
             (40%positive,AWeaken,7%positive)::nil
|}.

Definition set_file_type_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 3%positive => (-1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) <= 0 /\ -1 * (s IDset_file_type_n) <= 0)%Z
    | 4%positive => (-1 * (s IDset_file_type_n) <= 0 /\ 1 * (s IDset_file_type_n) <= 0 /\ 1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 5%positive => (-1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) <= 0 /\ -1 * (s IDset_file_type_n) <= 0 /\ 1 * (s IDset_file_type_bin_freq) <= 0 /\ -1 * (s IDset_file_type_bin_freq) <= 0)%Z
    | 6%positive => (-1 * (s IDset_file_type_bin_freq) <= 0 /\ 1 * (s IDset_file_type_bin_freq) <= 0 /\ -1 * (s IDset_file_type_n) <= 0 /\ 1 * (s IDset_file_type_n) <= 0 /\ 1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 7%positive => (-1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 8%positive => (1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 7 <= 0)%Z
    | 9%positive => (-1 * (s IDset_file_type_n) + 7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 10%positive => (1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 7 <= 0)%Z
    | 11%positive => (-1 * (s IDset_file_type_n) + 7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -128 <= 0)%Z
    | 12%positive => (1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 128 <= 0)%Z
    | 13%positive => (-1 * (s IDset_file_type_n) + 128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -128 <= 0)%Z
    | 14%positive => (1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 128 <= 0)%Z
    | 15%positive => (-1 * (s IDset_file_type_n) + 128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0)%Z
    | 16%positive => (1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 256 <= 0)%Z
    | 17%positive => (-1 * (s IDset_file_type_n) + 256 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0)%Z
    | 18%positive => (1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 256 <= 0)%Z
    | 19%positive => (-1 * (s IDset_file_type_n) + 256 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0)%Z
    | 20%positive => (-1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 128 <= 0 /\ 1 * (s IDset_file_type_n) + -255 <= 0)%Z
    | 21%positive => (1 * (s IDset_file_type_n) + -255 <= 0 /\ -1 * (s IDset_file_type_n) + 128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 22%positive => (-1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_n) + 129 <= 0)%Z
    | 23%positive => (-1 * (s IDset_file_type_n) + 129 <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 24%positive => (-1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_n) + 129 <= 0)%Z
    | 25%positive => (-1 * (s IDset_file_type_n) + 129 <= 0 /\ 1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 26%positive => (1 * (s IDset_file_type_n) + -256 <= 0 /\ -1 * (s IDset_file_type_n) + 129 <= 0 /\ -1 * (s IDset_file_type_z) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) + 7 <= 0 /\ 1 * (s IDset_file_type_n) + -127 <= 0)%Z
    | 28%positive => (1 * (s IDset_file_type_n) + -127 <= 0 /\ -1 * (s IDset_file_type_n) + 7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 29%positive => (-1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_n) + 8 <= 0)%Z
    | 30%positive => (-1 * (s IDset_file_type_n) + 8 <= 0 /\ 1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 31%positive => (-1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_n) + 8 <= 0)%Z
    | 32%positive => (-1 * (s IDset_file_type_n) + 8 <= 0 /\ 1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_z) <= 0)%Z
    | 33%positive => (1 * (s IDset_file_type_n) + -128 <= 0 /\ -1 * (s IDset_file_type_n) + 8 <= 0 /\ -1 * (s IDset_file_type_z) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_n) <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -6 <= 0)%Z
    | 35%positive => (1 * (s IDset_file_type_n) + -6 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_n) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 36%positive => (1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_n) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDset_file_type_n) + 1 <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 38%positive => (1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_n) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDset_file_type_n) + 1 <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_z) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_ascii_freq) <= 0)%Z
    | 40%positive => (1 * (s IDset_file_type_ascii_freq) <= 0 /\ -1 * (s IDset_file_type_ascii_freq) <= 0 /\ 1 * (s IDset_file_type_n) + -7 <= 0 /\ -1 * (s IDset_file_type_n) + 1 <= 0 /\ -1 * (s IDset_file_type_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition set_file_type_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + max0((s IDset_file_type_z)))%Q
    | 3%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 4%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 5%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 6%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 7%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 8%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 9%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                     + (1 # 2) * max0(128 - (s IDset_file_type_n))
                     + max0((s IDset_file_type_z)))%Q
    | 10%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                      + (1 # 2) * max0(128 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 11%positive => ((192 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (s IDset_file_type_z)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n)))%Q
    | 12%positive => ((192 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (s IDset_file_type_z)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n)))%Q
    | 13%positive => ((192 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (s IDset_file_type_z)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n)))%Q
    | 14%positive => ((192 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (s IDset_file_type_z)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n)))%Q
    | 15%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 16%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 17%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 18%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 19%positive => ((s IDset_file_type_z))%Q
    | 20%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 21%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 22%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 23%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 24%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 25%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 26%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 27%positive => ((192 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (s IDset_file_type_z)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n)))%Q
    | 28%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 29%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 30%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 31%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 32%positive => ((257 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 33%positive => ((256 # 1) - (s IDset_file_type_n)
                      + (s IDset_file_type_z))%Q
    | 34%positive => ((257 # 2) + (1 # 2) * max0(127 - (s IDset_file_type_n))
                      + (1 # 2) * max0(128 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 35%positive => ((385 # 2) - (1 # 2) * (s IDset_file_type_n)
                      + (1 # 2) * max0(127 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 36%positive => ((193 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 37%positive => ((193 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 38%positive => ((193 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 39%positive => ((193 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + (1 # 2) * max0(128 - (s IDset_file_type_n))
                      + max0((s IDset_file_type_z)))%Q
    | 40%positive => ((193 # 1) - (1 # 2) * (s IDset_file_type_n)
                      + max0(-1 + (s IDset_file_type_z))
                      + (1 # 2) * max0(128 - (s IDset_file_type_n)))%Q
    | _ => (0 # 1)%Q
  end.

Definition set_file_type_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDset_file_type_z))) (F_check_ge ((s IDset_file_type_z)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (127
                                                                    - 
                                                                    (s IDset_file_type_n))) (F_check_ge (127
                                                                    - (s IDset_file_type_n)) (0))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (128
                                                                    - 
                                                                    (s IDset_file_type_n))) (F_check_ge (128
                                                                    - (s IDset_file_type_n)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDset_file_type_n)) (255
                                                                    - (s IDset_file_type_n)));
                      (*-1 0*) F_max0_ge_0 (255 - (s IDset_file_type_n));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - (s IDset_file_type_n)) (0))) (F_max0_ge_0 (256
                                                                    - (s IDset_file_type_n)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (128
                                                                    - 
                                                                    (s IDset_file_type_n))) (F_check_ge (128
                                                                    - (s IDset_file_type_n)) (0))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (128
                                                                    - (s IDset_file_type_n)) (0))) (F_max0_ge_0 (128
                                                                    - (s IDset_file_type_n)))]
    | 34%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (128
                                                                    - 
                                                                    (s IDset_file_type_n))) (F_check_ge (128
                                                                    - (s IDset_file_type_n)) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDset_file_type_z)) (0))) (F_max0_ge_0 ((s IDset_file_type_z)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (127
                                                                    - (s IDset_file_type_n)) (0))) (F_max0_ge_0 (127
                                                                    - (s IDset_file_type_n)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDset_file_type_z))) (F_check_ge (-1
                                                                    + (s IDset_file_type_z)) (0))]
    | _ => []
  end.


Theorem set_file_type_ai_correct:
  forall s p' s', steps (g_start set_file_type) s (g_edges set_file_type) p' s' -> set_file_type_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem set_file_type_pot_correct:
  forall s p' s',
    steps (g_start set_file_type) s (g_edges set_file_type) p' s' ->
    (set_file_type_pot (g_start set_file_type) s >= set_file_type_pot p' s')%Q.
Proof.
  check_lp set_file_type_ai_correct set_file_type_hints.
Qed.

