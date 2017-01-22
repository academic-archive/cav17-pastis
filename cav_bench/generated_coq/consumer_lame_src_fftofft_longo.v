Require Import pasta.Pasta.

Notation IDfft_long_z := 1%positive.
Notation IDfft_long__tmp := 2%positive.
Notation IDfft_long_i := 3%positive.
Notation IDfft_long_jj := 4%positive.
Notation IDfft_long_buffer := 5%positive.
Notation IDfft_long_chn := 6%positive.
Notation IDfft_long_x := 7%positive.
Definition fft_long : graph := {|
  g_start := 1%positive;
  g_end := 44%positive;
  g_edges := (1%positive,(AAssign IDfft_long_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDfft_long__tmp
             (Some (EVar IDfft_long_chn))),3%positive)::
             (3%positive,(AAssign IDfft_long_jj (Some (ENum (127)))),
             4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDfft_long__tmp) s) <
             (eval (ENum (2)) s))%Z)),34%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDfft_long__tmp)
             s) >= (eval (ENum (2)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDfft_long__tmp) s) =
             (eval (ENum (2)) s))%Z)),20%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDfft_long__tmp)
             s) <> (eval (ENum (2)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDfft_long_i None),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDfft_long_jj
             (Some (EAdd (EVar IDfft_long_jj) (ENum (-1))))),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_long_jj)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),17%positive)::
             (14%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_long_jj)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,29%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDfft_long_z (Some (EAdd (ENum (1))
             (EVar IDfft_long_z)))),10%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDfft_long_i None),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDfft_long_jj
             (Some (EAdd (EVar IDfft_long_jj) (ENum (-1))))),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_long_jj)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),31%positive)::
             (26%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_long_jj)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,44%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDfft_long_z (Some (EAdd (ENum (1))
             (EVar IDfft_long_z)))),22%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDfft_long_i None),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDfft_long_jj
             (Some (EAdd (EVar IDfft_long_jj) (ENum (-1))))),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_long_jj)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),45%positive)::
             (40%positive,(AGuard (fun s => ((eval (EAdd (EVar IDfft_long_jj)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,44%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDfft_long_z (Some (EAdd (ENum (1))
             (EVar IDfft_long_z)))),36%positive)::nil
|}.

Definition fft_long_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_z) <= 0)%Z
    | 4%positive => (1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0)%Z
    | 5%positive => (-1 * (s IDfft_long_jj) + 127 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_z) <= 0)%Z
    | 6%positive => (1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0)%Z
    | 7%positive => (-1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_z) <= 0)%Z
    | 8%positive => (1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ -1 * (s IDfft_long__tmp) + 3 <= 0)%Z
    | 9%positive => (-1 * (s IDfft_long__tmp) + 3 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_z) <= 0)%Z
    | 10%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ -1 * (s IDfft_long__tmp) + 3 <= 0)%Z
    | 11%positive => (-1 * (s IDfft_long__tmp) + 3 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0)%Z
    | 12%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ -1 * (s IDfft_long__tmp) + 3 <= 0)%Z
    | 13%positive => (-1 * (s IDfft_long__tmp) + 3 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) <= 0)%Z
    | 14%positive => (-1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 3 <= 0)%Z
    | 15%positive => (-1 * (s IDfft_long__tmp) + 3 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) <= 0)%Z
    | 16%positive => (1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 3 <= 0)%Z
    | 17%positive => (-1 * (s IDfft_long__tmp) + 3 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 3 <= 0)%Z
    | 19%positive => (-1 * (s IDfft_long__tmp) + 3 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ 1 * (s IDfft_long__tmp) + -2 <= 0)%Z
    | 21%positive => (1 * (s IDfft_long__tmp) + -2 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_z) <= 0)%Z
    | 22%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ 1 * (s IDfft_long__tmp) + -2 <= 0)%Z
    | 23%positive => (1 * (s IDfft_long__tmp) + -2 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0)%Z
    | 24%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ 1 * (s IDfft_long__tmp) + -2 <= 0)%Z
    | 25%positive => (1 * (s IDfft_long__tmp) + -2 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) <= 0)%Z
    | 26%positive => (-1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ 1 * (s IDfft_long__tmp) + -2 <= 0)%Z
    | 27%positive => (1 * (s IDfft_long__tmp) + -2 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) <= 0)%Z
    | 28%positive => (1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ 1 * (s IDfft_long__tmp) + -2 <= 0)%Z
    | 29%positive => (-1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) <= 0)%Z
    | 30%positive => (1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0)%Z
    | 31%positive => (1 * (s IDfft_long__tmp) + -2 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ 1 * (s IDfft_long__tmp) + -2 <= 0)%Z
    | 33%positive => (1 * (s IDfft_long__tmp) + -2 <= 0 /\ -1 * (s IDfft_long__tmp) + 2 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ 1 * (s IDfft_long__tmp) + -1 <= 0)%Z
    | 35%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_jj) + 127 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_z) <= 0)%Z
    | 36%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long__tmp) + -1 <= 0)%Z
    | 37%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_z) <= 0)%Z
    | 38%positive => (-1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -127 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long__tmp) + -1 <= 0)%Z
    | 39%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) <= 0)%Z
    | 40%positive => (-1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long__tmp) + -1 <= 0)%Z
    | 41%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) <= 0)%Z
    | 42%positive => (1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long__tmp) + -1 <= 0)%Z
    | 43%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ 1 * (s IDfft_long_jj) <= 0)%Z
    | 44%positive => (1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_jj) <= 0 /\ -1 * (s IDfft_long_z) <= 0)%Z
    | 45%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDfft_long_jj) + 1 <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long__tmp) + -1 <= 0)%Z
    | 47%positive => (1 * (s IDfft_long__tmp) + -1 <= 0 /\ -1 * (s IDfft_long_z) <= 0 /\ 1 * (s IDfft_long_jj) + -126 <= 0 /\ -1 * (s IDfft_long_jj) + 1 <= 0)%Z
    | _ => False
  end.

Definition fft_long_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((126 # 1))%Q
    | 2%positive => ((126 # 1) + (s IDfft_long_z))%Q
    | 3%positive => ((126 # 1) + (s IDfft_long_z))%Q
    | 4%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj))
                     + (31 # 63) * max0(127 - (s IDfft_long_jj)))%Q
    | 5%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 6%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 7%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 8%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 9%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 10%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 11%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 12%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 13%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 14%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 15%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 16%positive => ((s IDfft_long_z))%Q
    | 17%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 18%positive => ((1 # 1) + (s IDfft_long_z)
                      + max0(-1 + (s IDfft_long_jj)))%Q
    | 19%positive => ((1 # 1) + (s IDfft_long_z)
                      + max0(-1 + (s IDfft_long_jj)))%Q
    | 20%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 21%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 22%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 23%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 24%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 25%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 26%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 27%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 28%positive => ((s IDfft_long_z))%Q
    | 29%positive => ((s IDfft_long_z))%Q
    | 30%positive => ((s IDfft_long_z))%Q
    | 31%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 32%positive => ((1 # 1) + (s IDfft_long_z)
                      + max0(-1 + (s IDfft_long_jj)))%Q
    | 33%positive => ((1 # 1) + (s IDfft_long_z)
                      + max0(-1 + (s IDfft_long_jj)))%Q
    | 34%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 35%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 36%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 37%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 38%positive => ((s IDfft_long_z) + max0(-1 + (s IDfft_long_jj)))%Q
    | 39%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 40%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 41%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 42%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 43%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 44%positive => ((s IDfft_long_z))%Q
    | 45%positive => ((s IDfft_long_z) + max0((s IDfft_long_jj)))%Q
    | 46%positive => ((1 # 1) + (s IDfft_long_z)
                      + max0(-1 + (s IDfft_long_jj)))%Q
    | 47%positive => ((1 # 1) + (s IDfft_long_z)
                      + max0(-1 + (s IDfft_long_jj)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fft_long_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-0.492126 0*) F_binom_monotonic 1 (F_max0_ge_0 (127
                                                                    - (s IDfft_long_jj))) (F_check_ge (0) (0))]
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
    | 15%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDfft_long_jj))) (F_check_ge (0) (0))]
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfft_long_jj)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfft_long_jj)) (-1
                                                                    + (s IDfft_long_jj)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDfft_long_jj))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfft_long_jj)) (1)]
    | 32%positive => []
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
    | 43%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfft_long_jj)) (-1
                                                                    + (s IDfft_long_jj)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDfft_long_jj))]
    | 44%positive => []
    | 45%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfft_long_jj)) (1)]
    | 46%positive => []
    | 47%positive => []
    | _ => []
  end.


Theorem fft_long_ai_correct:
  forall s p' s', steps (g_start fft_long) s (g_edges fft_long) p' s' -> fft_long_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fft_long_pot_correct:
  forall s p' s',
    steps (g_start fft_long) s (g_edges fft_long) p' s' ->
    (fft_long_pot (g_start fft_long) s >= fft_long_pot p' s')%Q.
Proof.
  check_lp fft_long_ai_correct fft_long_hints.
Qed.

