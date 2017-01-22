Require Import pasta.Pasta.

Notation IDgetline_z := 1%positive.
Notation IDgetline__tmp := 2%positive.
Notation IDgetline__tmp1 := 3%positive.
Notation IDgetline_c := 4%positive.
Notation IDgetline_state := 5%positive.
Notation IDgetline_buf := 6%positive.
Notation IDgetline_f := 7%positive.
Notation IDgetline_n := 8%positive.
Definition getline : graph := {|
  g_start := 1%positive;
  g_end := 43%positive;
  g_edges := (1%positive,(AAssign IDgetline_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgetline__tmp1 (Some (EVar IDgetline_n))),
             3%positive)::
             (3%positive,(AAssign IDgetline_state (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDgetline_c None),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDgetline_c) s) =
             (eval (ENum (10)) s))%Z)),39%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDgetline_c) s) <>
             (eval (ENum (10)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDgetline_state)
             s) <> (eval (ENum (0)) s))%Z)),35%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDgetline_state) s) =
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDgetline_c) s) =
             (eval (ENum (-1)) s))%Z)),31%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDgetline_c) s) <>
             (eval (ENum (-1)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDgetline_c) s) =
             (eval (ENum (13)) s))%Z)),25%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDgetline_c) s) <>
             (eval (ENum (13)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDgetline__tmp1
             (Some (EAdd (EVar IDgetline__tmp1) (ENum (-1))))),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDgetline__tmp1) (ENum (-1))) s) >
             (eval (ENum (0)) s))%Z)),22%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDgetline__tmp1) (ENum (-1))) s) <=
             (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDgetline__tmp (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,43%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,28%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDgetline_state (Some (ENum (1)))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDgetline_z (Some (EAdd (ENum (1))
             (EVar IDgetline_z)))),5%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AAssign IDgetline__tmp None),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,43%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDgetline__tmp (Some (ENum (1)))),
             37%positive)::(37%positive,ANone,38%positive)::
             (38%positive,AWeaken,43%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDgetline__tmp (Some (ENum (1)))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::nil
|}.

Definition getline_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_z) <= 0)%Z
    | 4%positive => (1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 5%positive => (-1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 6%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 8%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 10%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0)%Z
    | 11%positive => (1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 12%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0)%Z
    | 13%positive => (1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 14%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0)%Z
    | 15%positive => (1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 16%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0)%Z
    | 17%positive => (1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 18%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline__tmp1) + -1 <= 0)%Z
    | 19%positive => (1 * (s IDgetline__tmp1) + -1 <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 20%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline__tmp1) + -1 <= 0 /\ 1 * (s IDgetline__tmp) <= 0 /\ -1 * (s IDgetline__tmp) <= 0)%Z
    | 21%positive => (-1 * (s IDgetline__tmp) <= 0 /\ 1 * (s IDgetline__tmp) <= 0 /\ 1 * (s IDgetline__tmp1) + -1 <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 22%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline__tmp1) + 2 <= 0)%Z
    | 23%positive => (-1 * (s IDgetline__tmp1) + 2 <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 24%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline__tmp1) + 2 <= 0)%Z
    | 25%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_c) + -13 <= 0 /\ -1 * (s IDgetline_c) + 13 <= 0)%Z
    | 26%positive => (-1 * (s IDgetline_c) + 13 <= 0 /\ 1 * (s IDgetline_c) + -13 <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 27%positive => (-1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_c) + -13 <= 0 /\ -1 * (s IDgetline_c) + 13 <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 29%positive => (-1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 30%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 31%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_c) + 1 <= 0 /\ -1 * (s IDgetline_c) + -1 <= 0)%Z
    | 32%positive => (-1 * (s IDgetline_c) + -1 <= 0 /\ 1 * (s IDgetline_c) + 1 <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 33%positive => (-1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_c) + 1 <= 0 /\ -1 * (s IDgetline_c) + -1 <= 0)%Z
    | 34%positive => (-1 * (s IDgetline_c) + -1 <= 0 /\ 1 * (s IDgetline_c) + 1 <= 0 /\ 1 * (s IDgetline_state) <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 35%positive => (-1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) + 1 <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0)%Z
    | 36%positive => (1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) + 1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 37%positive => (-1 * (s IDgetline_z) <= 0 /\ -1 * (s IDgetline_state) + 1 <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ 1 * (s IDgetline__tmp) + -1 <= 0 /\ -1 * (s IDgetline__tmp) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDgetline__tmp) + 1 <= 0 /\ 1 * (s IDgetline__tmp) + -1 <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) + 1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | 39%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_c) + -10 <= 0 /\ -1 * (s IDgetline_c) + 10 <= 0)%Z
    | 40%positive => (-1 * (s IDgetline_c) + 10 <= 0 /\ 1 * (s IDgetline_c) + -10 <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 41%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_c) + -10 <= 0 /\ -1 * (s IDgetline_c) + 10 <= 0 /\ 1 * (s IDgetline__tmp) + -1 <= 0 /\ -1 * (s IDgetline__tmp) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDgetline__tmp) + 1 <= 0 /\ 1 * (s IDgetline__tmp) + -1 <= 0 /\ -1 * (s IDgetline_c) + 10 <= 0 /\ 1 * (s IDgetline_c) + -10 <= 0 /\ -1 * (s IDgetline_z) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_state) <= 0)%Z
    | 43%positive => (-1 * (s IDgetline_state) <= 0 /\ 1 * (s IDgetline_state) + -1 <= 0 /\ -1 * (s IDgetline_z) <= 0)%Z
    | _ => False
  end.

Definition getline_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 1) + max0(-2 + (s IDgetline_n)))%Q
    | 2%positive => ((1 # 1) + (s IDgetline_z) + max0(-2 + (s IDgetline_n)))%Q
    | 3%positive => ((1 # 1) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1)))%Q
    | 4%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1))
                     + max0(-1 + (s IDgetline_state)))%Q
    | 5%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1))
                     + max0(-1 + (s IDgetline_state)))%Q
    | 6%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1))
                     + max0(-1 + (s IDgetline_state)))%Q
    | 7%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1))
                     + max0(-1 + (s IDgetline_state)))%Q
    | 8%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1))
                     + max0(-1 + (s IDgetline_state)))%Q
    | 9%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                     + max0(-2 + (s IDgetline__tmp1))
                     + max0(-1 + (s IDgetline_state)))%Q
    | 10%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 11%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 12%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 13%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 14%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 15%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 16%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 17%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 18%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 19%positive => ((1 # 1) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state))
                      + max0(-(s IDgetline_state)))%Q
    | 20%positive => ((1 # 1) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state))
                      + max0(-(s IDgetline_state)))%Q
    | 21%positive => ((1 # 1) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state))
                      + max0(-(s IDgetline_state)))%Q
    | 22%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-1 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 23%positive => ((2 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 24%positive => ((2 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 25%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 26%positive => ((1 # 1) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1)))%Q
    | 27%positive => ((2 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 28%positive => ((2 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 29%positive => ((2 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 30%positive => ((2 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 31%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 32%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 33%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 34%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 35%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 36%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 37%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 38%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 39%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 40%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 41%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 42%positive => ((1 # 1) - (s IDgetline_state) + (s IDgetline_z)
                      + max0(-2 + (s IDgetline__tmp1))
                      + max0(-1 + (s IDgetline_state)))%Q
    | 43%positive => ((s IDgetline_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition getline_hints (p : node) (s : state) := 
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
    | 18%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgetline_state)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgetline_state)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_monotonic (F_check_ge (-1
                                                             + (s IDgetline__tmp1)) (-2
                                                                    + (s IDgetline__tmp1)));
                      (*-1 0*) F_max0_ge_0 (-2 + (s IDgetline__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgetline_state))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgetline_state))) (F_check_ge (0) (0))]
    | 22%positive => [(*-1 0*) F_max0_pre_decrement (-1 + (s IDgetline__tmp1)) (1)]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDgetline_state))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDgetline_state)) (0))) (F_max0_ge_0 (-
                                                                    (s IDgetline_state)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgetline_state))) (F_check_ge (0) (0))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_max0_ge_0 (-2 + (s IDgetline__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDgetline_state))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDgetline_state)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDgetline_state)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgetline_state))) (F_check_ge (0) (0))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_max0_ge_0 (-2 + (s IDgetline__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDgetline_state))) (F_check_ge (-1
                                                                    + (s IDgetline_state)) (0))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_max0_ge_0 (-2 + (s IDgetline__tmp1));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDgetline_state))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDgetline_state)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDgetline_state)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDgetline_state))) (F_check_ge (0) (0))]
    | 43%positive => []
    | _ => []
  end.


Theorem getline_ai_correct:
  forall s p' s', steps (g_start getline) s (g_edges getline) p' s' -> getline_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem getline_pot_correct:
  forall s p' s',
    steps (g_start getline) s (g_edges getline) p' s' ->
    (getline_pot (g_start getline) s >= getline_pot p' s')%Q.
Proof.
  check_lp getline_ai_correct getline_hints.
Qed.

