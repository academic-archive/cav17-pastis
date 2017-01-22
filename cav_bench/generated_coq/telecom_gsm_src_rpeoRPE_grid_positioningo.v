Require Import pasta.Pasta.

Notation IDRPE_grid_positioning_z := 1%positive.
Notation IDRPE_grid_positioning__tmp := 2%positive.
Notation IDRPE_grid_positioning_i := 3%positive.
Notation IDRPE_grid_positioning_Mc := 4%positive.
Notation IDRPE_grid_positioning_ep := 5%positive.
Notation IDRPE_grid_positioning_xMp := 6%positive.
Definition RPE_grid_positioning : graph := {|
  g_start := 1%positive;
  g_end := 34%positive;
  g_edges := (1%positive,(AAssign IDRPE_grid_positioning_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDRPE_grid_positioning__tmp
             (Some (EVar IDRPE_grid_positioning_Mc))),3%positive)::
             (3%positive,(AAssign IDRPE_grid_positioning_i
             (Some (ENum (13)))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (ENum (0)) s) <=
             (eval (EVar IDRPE_grid_positioning__tmp) s))%Z)),7%positive)::
             (5%positive,(AGuard (fun s => ((eval (ENum (0)) s) >
             (eval (EVar IDRPE_grid_positioning__tmp) s))%Z)),6%positive)::
             (6%positive,AWeaken,10%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_positioning__tmp) s) <=
             (eval (ENum (3)) s))%Z)),12%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDRPE_grid_positioning__tmp) s) >
             (eval (ENum (3)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,34%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,29%positive)::
             (15%positive,ANone,16%positive)::
             (15%positive,ANone,17%positive)::
             (15%positive,ANone,19%positive)::
             (15%positive,ANone,20%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDRPE_grid_positioning_i
             (Some (EAdd (EVar IDRPE_grid_positioning_i) (ENum (-1))))),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDRPE_grid_positioning_i)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),26%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDRPE_grid_positioning_i)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,29%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDRPE_grid_positioning_z
             (Some (EAdd (ENum (1)) (EVar IDRPE_grid_positioning_z)))),
             18%positive)::(29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDRPE_grid_positioning__tmp
             (Some (EAdd (EVar IDRPE_grid_positioning__tmp) (ENum (1))))),
             31%positive)::(31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDRPE_grid_positioning__tmp)
             (ENum (1))) s) < (eval (ENum (4)) s))%Z)),35%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDRPE_grid_positioning__tmp)
             (ENum (1))) s) >= (eval (ENum (4)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDRPE_grid_positioning_z
             (Some (EAdd (ENum (1)) (EVar IDRPE_grid_positioning_z)))),
             30%positive)::nil
|}.

Definition RPE_grid_positioning_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 3%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 4%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0)%Z
    | 5%positive => (-1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 6%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 9%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 4 <= 0)%Z
    | 10%positive => (-1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 11%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0)%Z
    | 12%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0)%Z
    | 13%positive => (1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 14%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 16%positive => (1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0)%Z
    | 17%positive => (1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 13 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 18%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0)%Z
    | 19%positive => (-1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 20%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0)%Z
    | 21%positive => (-1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 22%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -12 <= 0)%Z
    | 23%positive => (1 * (s IDRPE_grid_positioning_i) + -12 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 24%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -1 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDRPE_grid_positioning_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -1 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 26%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -12 <= 0)%Z
    | 27%positive => (1 * (s IDRPE_grid_positioning_i) + -12 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0)%Z
    | 28%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -12 <= 0)%Z
    | 29%positive => (1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDRPE_grid_positioning__tmp) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -3 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDRPE_grid_positioning_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -4 <= 0)%Z
    | 32%positive => (1 * (s IDRPE_grid_positioning__tmp) + -4 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDRPE_grid_positioning_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -4 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 3 <= 0)%Z
    | 34%positive => (-1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDRPE_grid_positioning_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -2 <= 0)%Z
    | 36%positive => (1 * (s IDRPE_grid_positioning__tmp) + -2 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDRPE_grid_positioning_i) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0 /\ 1 * (s IDRPE_grid_positioning__tmp) + -2 <= 0)%Z
    | 38%positive => (1 * (s IDRPE_grid_positioning__tmp) + -2 <= 0 /\ -1 * (s IDRPE_grid_positioning__tmp) + 1 <= 0 /\ -1 * (s IDRPE_grid_positioning_z) <= 0 /\ 1 * (s IDRPE_grid_positioning_i) + -13 <= 0 /\ -1 * (s IDRPE_grid_positioning_i) + 1 <= 0)%Z
    | _ => False
  end.

Definition RPE_grid_positioning_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((15 # 4) * max0(4 - (s IDRPE_grid_positioning_Mc))
                     + (11 # 4) * max0((s IDRPE_grid_positioning_Mc)))%Q
    | 2%positive => ((s IDRPE_grid_positioning_z)
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning_Mc))
                     + (11 # 4) * max0((s IDRPE_grid_positioning_Mc)))%Q
    | 3%positive => ((s IDRPE_grid_positioning_z)
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 4%positive => (-(11 # 4) + (s IDRPE_grid_positioning_z)
                     + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                     + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 5%positive => (-(11 # 4) + (s IDRPE_grid_positioning_z)
                     + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                     + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 6%positive => (-(11 # 4) + (s IDRPE_grid_positioning_z)
                     + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                     + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 7%positive => (-(11 # 4) + (s IDRPE_grid_positioning_z)
                     + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                     + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 8%positive => (-(5 # 3) - (1 # 12) * (s IDRPE_grid_positioning_i)
                     + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                     + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp))
                     + max0((s IDRPE_grid_positioning_z)))%Q
    | 9%positive => (-(5 # 3) - (1 # 12) * (s IDRPE_grid_positioning_i)
                     + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                     + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                     + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                     - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                     + (11 # 4) * max0((s IDRPE_grid_positioning__tmp))
                     + max0((s IDRPE_grid_positioning_z)))%Q
    | 10%positive => (max0((s IDRPE_grid_positioning_z)))%Q
    | 11%positive => (max0((s IDRPE_grid_positioning_z)))%Q
    | 12%positive => (-(5 # 3) - (1 # 12) * (s IDRPE_grid_positioning_i)
                      + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                      + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                      + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp))
                      + max0((s IDRPE_grid_positioning_z)))%Q
    | 13%positive => (-(5 # 3) - (1 # 12) * (s IDRPE_grid_positioning_i)
                      + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                      + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                      + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp))
                      + max0((s IDRPE_grid_positioning_z)))%Q
    | 14%positive => (-(5 # 3) - (1 # 12) * (s IDRPE_grid_positioning_i)
                      + (37 # 48) * max0(-13 + (s IDRPE_grid_positioning_i))
                      + (11 # 48) * max0(-1 + (s IDRPE_grid_positioning_i))
                      + (15 # 4) * max0(4 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp))
                      + max0((s IDRPE_grid_positioning_z)))%Q
    | 15%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 16%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 17%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 18%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 19%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 20%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 21%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 22%positive => ((1 # 1) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(12 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 23%positive => (-(11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 24%positive => (-(11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 25%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 26%positive => (-(11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 27%positive => ((13 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 28%positive => ((13 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 29%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 30%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 31%positive => ((17 # 6) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + (11 # 4) * max0(-1 + (s IDRPE_grid_positioning__tmp))
                      + max0(3 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i)))%Q
    | 32%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(3 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 33%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(3 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 34%positive => ((s IDRPE_grid_positioning_z))%Q
    | 35%positive => ((1 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(3 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 36%positive => ((13 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 37%positive => ((13 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | 38%positive => ((13 # 12) - (11 # 4) * (s IDRPE_grid_positioning__tmp)
                      + (11 # 12) * (s IDRPE_grid_positioning_i)
                      + (s IDRPE_grid_positioning_z)
                      + max0(2 - (s IDRPE_grid_positioning__tmp))
                      - (1 # 12) * max0(13 - (s IDRPE_grid_positioning_i))
                      + (11 # 4) * max0((s IDRPE_grid_positioning__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition RPE_grid_positioning_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 1.375*) F_max0_ge_0 (2
                                              - (s IDRPE_grid_positioning__tmp));
                     (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDRPE_grid_positioning_z)) (0))) (F_max0_ge_0 ((s IDRPE_grid_positioning_z)));
                     (*0 2.75*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDRPE_grid_positioning__tmp))) (F_check_ge (0) (0));
                     (*-2.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                    - 
                                                                    (s IDRPE_grid_positioning__tmp))) (F_check_ge (0) (0));
                     (*-1.375 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                    - (s IDRPE_grid_positioning__tmp))) (F_check_ge (4
                                                                    - (s IDRPE_grid_positioning__tmp)) (0));
                     (*-1.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDRPE_grid_positioning__tmp)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDRPE_grid_positioning__tmp)));
                     (*0 0.229167*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0));
                     (*0 0.770833*) F_binom_monotonic 1 (F_max0_ge_0 (-13
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0))]
    | 7%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDRPE_grid_positioning_z)) (0))) (F_max0_ge_0 ((s IDRPE_grid_positioning_z)));
                     (*0 0.0833333*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                                    - (s IDRPE_grid_positioning_i))) (F_check_ge (13
                                                                    - (s IDRPE_grid_positioning_i)) (0))]
    | 8%positive => []
    | 9%positive => [(*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDRPE_grid_positioning__tmp))) (F_check_ge ((s IDRPE_grid_positioning__tmp)) (0));
                     (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDRPE_grid_positioning_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDRPE_grid_positioning_i)));
                     (*-3.75 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                   - 
                                                                   (s IDRPE_grid_positioning__tmp))) (F_check_ge (0) (0));
                     (*-0.229167 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0));
                     (*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   (s IDRPE_grid_positioning__tmp))) (F_check_ge (0) (0));
                     (*-2.75 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDRPE_grid_positioning__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDRPE_grid_positioning__tmp)));
                     (*-0.770833 0*) F_binom_monotonic 1 (F_max0_ge_0 (-13
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDRPE_grid_positioning_z))) (F_check_ge ((s IDRPE_grid_positioning_z)) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDRPE_grid_positioning__tmp)) (2
                                                                    - (s IDRPE_grid_positioning__tmp)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDRPE_grid_positioning_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDRPE_grid_positioning_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDRPE_grid_positioning_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDRPE_grid_positioning_z)));
                      (*-3.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                    - (s IDRPE_grid_positioning__tmp))) (F_check_ge (4
                                                                    - (s IDRPE_grid_positioning__tmp)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - (s IDRPE_grid_positioning__tmp)) (0))) (F_max0_ge_0 (3
                                                                    - (s IDRPE_grid_positioning__tmp)));
                      (*0 0.229167*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (-1
                                                                    + (s IDRPE_grid_positioning_i)) (0));
                      (*-0.770833 0*) F_binom_monotonic 1 (F_max0_ge_arg (-13
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (-13
                                                                    + (s IDRPE_grid_positioning_i)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (12
                                                                    - (s IDRPE_grid_positioning_i)) (0))) (F_max0_ge_0 (12
                                                                    - (s IDRPE_grid_positioning_i)))]
    | 23%positive => []
    | 24%positive => [(*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (13
                                                                    - (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDRPE_grid_positioning_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDRPE_grid_positioning_i)))]
    | 25%positive => []
    | 26%positive => [(*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                                    - (s IDRPE_grid_positioning_i))) (F_check_ge (13
                                                                    - (s IDRPE_grid_positioning_i)) (0))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*0 2.75*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDRPE_grid_positioning__tmp)) (0))) (F_max0_ge_0 ((s IDRPE_grid_positioning__tmp)));
                      (*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDRPE_grid_positioning__tmp))) (F_check_ge (-1
                                                                    + (s IDRPE_grid_positioning__tmp)) (0))]
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDRPE_grid_positioning__tmp)) (2
                                                                    - (s IDRPE_grid_positioning__tmp)));
                      (*-1 0*) F_max0_ge_0 (2
                                            - (s IDRPE_grid_positioning__tmp));
                      (*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDRPE_grid_positioning__tmp))) (F_check_ge ((s IDRPE_grid_positioning__tmp)) (0));
                      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - (s IDRPE_grid_positioning_i)) (0))) (F_max0_ge_0 (13
                                                                    - (s IDRPE_grid_positioning_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDRPE_grid_positioning_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDRPE_grid_positioning_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDRPE_grid_positioning_i)))]
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_pre_decrement (3
                                                     - (s IDRPE_grid_positioning__tmp)) (1)]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem RPE_grid_positioning_ai_correct:
  forall s p' s', steps (g_start RPE_grid_positioning) s (g_edges RPE_grid_positioning) p' s' -> RPE_grid_positioning_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem RPE_grid_positioning_pot_correct:
  forall s p' s',
    steps (g_start RPE_grid_positioning) s (g_edges RPE_grid_positioning) p' s' ->
    (RPE_grid_positioning_pot (g_start RPE_grid_positioning) s >= RPE_grid_positioning_pot p' s')%Q.
Proof.
  check_lp RPE_grid_positioning_ai_correct RPE_grid_positioning_hints.
Qed.

