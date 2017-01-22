Require Import pasta.Pasta.

Notation IDIII_imdct_s_z := 1%positive.
Notation IDIII_imdct_s_i := 2%positive.
Notation IDIII_imdct_s_lo := 3%positive.
Notation IDIII_imdct_s_w := 4%positive.
Notation IDIII_imdct_s_X := 5%positive.
Notation IDIII_imdct_s_z := 6%positive.
Definition III_imdct_s : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDIII_imdct_s_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDIII_imdct_s_w (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_s_w) s) <
             (eval (ENum (3)) s))%Z)),24%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_s_w)
             s) >= (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDIII_imdct_s_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_s_i)
             s) < (eval (ENum (6)) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_s_i)
             s) >= (eval (ENum (6)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDIII_imdct_s_lo None),15%positive)::
             (15%positive,(AAssign IDIII_imdct_s_lo None),16%positive)::
             (16%positive,(AAssign IDIII_imdct_s_lo None),17%positive)::
             (17%positive,(AAssign IDIII_imdct_s_lo None),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDIII_imdct_s_i
             (Some (EAdd (EVar IDIII_imdct_s_i) (ENum (1))))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDIII_imdct_s_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_s_z)))),23%positive)::
             (23%positive,AWeaken,10%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDIII_imdct_s_i (Some (ENum (0)))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_s_i)
             s) < (eval (ENum (3)) s))%Z)),36%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDIII_imdct_s_i)
             s) >= (eval (ENum (3)) s))%Z)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDIII_imdct_s_w
             (Some (EAdd (EVar IDIII_imdct_s_w) (ENum (1))))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDIII_imdct_s_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_s_z)))),35%positive)::
             (35%positive,AWeaken,5%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDIII_imdct_s_lo None),38%positive)::
             (38%positive,(AAssign IDIII_imdct_s_lo None),39%positive)::
             (39%positive,(AAssign IDIII_imdct_s_lo None),40%positive)::
             (40%positive,(AAssign IDIII_imdct_s_lo None),41%positive)::
             (41%positive,(AAssign IDIII_imdct_s_lo None),42%positive)::
             (42%positive,(AAssign IDIII_imdct_s_lo None),43%positive)::
             (43%positive,(AAssign IDIII_imdct_s_lo None),44%positive)::
             (44%positive,(AAssign IDIII_imdct_s_lo None),45%positive)::
             (45%positive,(AAssign IDIII_imdct_s_lo None),46%positive)::
             (46%positive,(AAssign IDIII_imdct_s_lo None),47%positive)::
             (47%positive,(AAssign IDIII_imdct_s_lo None),48%positive)::
             (48%positive,(AAssign IDIII_imdct_s_lo None),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDIII_imdct_s_i
             (Some (EAdd (EVar IDIII_imdct_s_i) (ENum (1))))),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDIII_imdct_s_z (Some (EAdd (ENum (1))
             (EVar IDIII_imdct_s_z)))),54%positive)::
             (54%positive,AWeaken,28%positive)::nil
|}.

Definition III_imdct_s_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0)%Z
    | 3%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 4%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ 1 * (s IDIII_imdct_s_w) <= 0 /\ 1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0)%Z
    | 5%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 6%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0)%Z
    | 7%positive => (-1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0)%Z
    | 8%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ 1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0)%Z
    | 9%positive => (-1 * (s IDIII_imdct_s_i) <= 0 /\ 1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0)%Z
    | 10%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -6 <= 0)%Z
    | 11%positive => (1 * (s IDIII_imdct_s_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) + 6 <= 0)%Z
    | 12%positive => (-1 * (s IDIII_imdct_s_i) + 6 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -6 <= 0)%Z
    | 13%positive => (-1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -5 <= 0)%Z
    | 14%positive => (1 * (s IDIII_imdct_s_i) + -5 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0)%Z
    | 15%positive => (-1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -5 <= 0)%Z
    | 16%positive => (1 * (s IDIII_imdct_s_i) + -5 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0)%Z
    | 17%positive => (-1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -5 <= 0)%Z
    | 18%positive => (1 * (s IDIII_imdct_s_i) + -5 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0)%Z
    | 19%positive => (-1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -5 <= 0)%Z
    | 20%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -6 <= 0)%Z
    | 21%positive => (1 * (s IDIII_imdct_s_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0)%Z
    | 22%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -6 <= 0)%Z
    | 23%positive => (1 * (s IDIII_imdct_s_i) + -6 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_s_w) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_w) + -2 <= 0)%Z
    | 25%positive => (1 * (s IDIII_imdct_s_w) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 26%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_w) + -2 <= 0 /\ 1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0)%Z
    | 27%positive => (-1 * (s IDIII_imdct_s_i) <= 0 /\ 1 * (s IDIII_imdct_s_i) <= 0 /\ 1 * (s IDIII_imdct_s_w) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 28%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0)%Z
    | 29%positive => (1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) + 3 <= 0)%Z
    | 30%positive => (-1 * (s IDIII_imdct_s_i) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0)%Z
    | 31%positive => (1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) + 3 <= 0)%Z
    | 32%positive => (-1 * (s IDIII_imdct_s_i) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_w) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDIII_imdct_s_w) + 1 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) + 3 <= 0)%Z
    | 34%positive => (-1 * (s IDIII_imdct_s_i) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_w) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDIII_imdct_s_w) + 1 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 3 <= 0 /\ -1 * (s IDIII_imdct_s_z) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 37%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 38%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 39%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 40%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 41%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 42%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 43%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 44%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 45%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 46%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 47%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 48%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 49%positive => (1 * (s IDIII_imdct_s_i) + -2 <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0)%Z
    | 50%positive => (-1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0 /\ 1 * (s IDIII_imdct_s_i) + -2 <= 0)%Z
    | 51%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0)%Z
    | 52%positive => (1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_z) <= 0)%Z
    | 53%positive => (-1 * (s IDIII_imdct_s_z) <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ 1 * (s IDIII_imdct_s_i) + -3 <= 0)%Z
    | 54%positive => (1 * (s IDIII_imdct_s_i) + -3 <= 0 /\ -1 * (s IDIII_imdct_s_i) + 1 <= 0 /\ -1 * (s IDIII_imdct_s_w) <= 0 /\ -1 * (s IDIII_imdct_s_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition III_imdct_s_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((18 # 1))%Q
    | 2%positive => ((18 # 1) + (s IDIII_imdct_s_z))%Q
    | 3%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 4%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 5%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 6%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 7%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 8%positive => ((s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w))
                     + max0(6 - (s IDIII_imdct_s_i)))%Q
    | 9%positive => ((s IDIII_imdct_s_z)
                     + (4 # 1) * max0(3 - (s IDIII_imdct_s_w))
                     + max0(6 - (s IDIII_imdct_s_i)))%Q
    | 10%positive => ((s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w))
                      + max0(6 - (s IDIII_imdct_s_i)))%Q
    | 11%positive => ((s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w))
                      + max0(6 - (s IDIII_imdct_s_i)))%Q
    | 12%positive => ((s IDIII_imdct_s_z))%Q
    | 13%positive => ((s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w))
                      + max0(6 - (s IDIII_imdct_s_i)))%Q
    | 14%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 15%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 16%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 17%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 18%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 19%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 20%positive => ((7 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 21%positive => ((7 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 22%positive => ((7 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 23%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 24%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 25%positive => ((6 # 1) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 26%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 27%positive => ((6 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 28%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 29%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 30%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 31%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 32%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 33%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 34%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 35%positive => ((9 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(3 - (s IDIII_imdct_s_w)))%Q
    | 36%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 37%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 38%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 39%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 40%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 41%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 42%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 43%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 44%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 45%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 46%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 47%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 48%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 49%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 50%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 51%positive => ((11 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 52%positive => ((11 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 53%positive => ((11 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | 54%positive => ((10 # 1) - (s IDIII_imdct_s_i) + (s IDIII_imdct_s_z)
                      + (4 # 1) * max0(2 - (s IDIII_imdct_s_w)))%Q
    | _ => (0 # 1)%Q
  end.

Definition III_imdct_s_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDIII_imdct_s_w)) (2
                                                                    - (s IDIII_imdct_s_w)));
                      (*-4 0*) F_max0_ge_0 (2 - (s IDIII_imdct_s_w));
                      (*-1 0*) F_max0_monotonic (F_check_ge (6
                                                             - (s IDIII_imdct_s_i)) (5
                                                                    - (s IDIII_imdct_s_i)));
                      (*-1 0*) F_max0_ge_0 (5 - (s IDIII_imdct_s_i))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                                   - 
                                                                   (s IDIII_imdct_s_i))) (F_check_ge (6
                                                                    - (s IDIII_imdct_s_i)) (0))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - (s IDIII_imdct_s_i)) (0))) (F_max0_ge_0 (6
                                                                    - (s IDIII_imdct_s_i)))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-4 0*) F_max0_pre_decrement (3 - (s IDIII_imdct_s_w)) (1)]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_ge_0 (3 - (s IDIII_imdct_s_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDIII_imdct_s_i)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDIII_imdct_s_i)))]
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
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | _ => []
  end.


Theorem III_imdct_s_ai_correct:
  forall s p' s', steps (g_start III_imdct_s) s (g_edges III_imdct_s) p' s' -> III_imdct_s_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem III_imdct_s_pot_correct:
  forall s p' s',
    steps (g_start III_imdct_s) s (g_edges III_imdct_s) p' s' ->
    (III_imdct_s_pot (g_start III_imdct_s) s >= III_imdct_s_pot p' s')%Q.
Proof.
  check_lp III_imdct_s_ai_correct III_imdct_s_hints.
Qed.

