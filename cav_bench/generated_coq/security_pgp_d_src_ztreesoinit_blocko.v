Require Import pasta.Pasta.

Notation IDinit_block_z := 1%positive.
Notation IDinit_block_flag_bit := 2%positive.
Notation IDinit_block_flags := 3%positive.
Notation IDinit_block_last_dist := 4%positive.
Notation IDinit_block_last_flags := 5%positive.
Notation IDinit_block_last_lit := 6%positive.
Notation IDinit_block_n := 7%positive.
Notation IDinit_block_opt_len := 8%positive.
Notation IDinit_block_static_len := 9%positive.
Definition init_block : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDinit_block_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDinit_block_n (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinit_block_n) s) <
             (eval (ENum (286)) s))%Z)),40%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDinit_block_n) s) >=
             (eval (ENum (286)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDinit_block_n (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDinit_block_n) s) <
             (eval (ENum (30)) s))%Z)),33%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDinit_block_n)
             s) >= (eval (ENum (30)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDinit_block_n (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDinit_block_n) s) <
             (eval (ENum (19)) s))%Z)),26%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDinit_block_n)
             s) >= (eval (ENum (19)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDinit_block_static_len
             (Some (ENum (0)))),18%positive)::
             (18%positive,(AAssign IDinit_block_opt_len (Some (ENum (0)))),
             19%positive)::
             (19%positive,(AAssign IDinit_block_last_flags
             (Some (ENum (0)))),20%positive)::
             (20%positive,(AAssign IDinit_block_last_dist (Some (ENum (0)))),
             21%positive)::
             (21%positive,(AAssign IDinit_block_last_lit (Some (ENum (0)))),
             22%positive)::
             (22%positive,(AAssign IDinit_block_flags (Some (ENum (0)))),
             23%positive)::
             (23%positive,(AAssign IDinit_block_flag_bit (Some (ENum (1)))),
             24%positive)::(24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDinit_block_n
             (Some (EAdd (EVar IDinit_block_n) (ENum (1))))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDinit_block_z (Some (EAdd (ENum (1))
             (EVar IDinit_block_z)))),32%positive)::
             (32%positive,AWeaken,15%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDinit_block_n
             (Some (EAdd (EVar IDinit_block_n) (ENum (1))))),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDinit_block_z (Some (EAdd (ENum (1))
             (EVar IDinit_block_z)))),39%positive)::
             (39%positive,AWeaken,10%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDinit_block_n
             (Some (EAdd (EVar IDinit_block_n) (ENum (1))))),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDinit_block_z (Some (EAdd (ENum (1))
             (EVar IDinit_block_z)))),46%positive)::
             (46%positive,AWeaken,5%positive)::nil
|}.

Definition init_block_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_n) <= 0)%Z
    | 4%positive => (-1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 5%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_n) + -286 <= 0)%Z
    | 6%positive => (1 * (s IDinit_block_n) + -286 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 286 <= 0)%Z
    | 7%positive => (-1 * (s IDinit_block_n) + 286 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -286 <= 0)%Z
    | 8%positive => (-1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_n) <= 0)%Z
    | 9%positive => (-1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 10%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_n) + -30 <= 0)%Z
    | 11%positive => (1 * (s IDinit_block_n) + -30 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 30 <= 0)%Z
    | 12%positive => (-1 * (s IDinit_block_n) + 30 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -30 <= 0)%Z
    | 13%positive => (-1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_n) <= 0)%Z
    | 14%positive => (-1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 15%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0)%Z
    | 16%positive => (1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0)%Z
    | 17%positive => (-1 * (s IDinit_block_n) + 19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0)%Z
    | 18%positive => (1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0)%Z
    | 19%positive => (-1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ -1 * (s IDinit_block_opt_len) <= 0)%Z
    | 20%positive => (-1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_last_flags) <= 0 /\ -1 * (s IDinit_block_last_flags) <= 0)%Z
    | 21%positive => (-1 * (s IDinit_block_last_flags) <= 0 /\ 1 * (s IDinit_block_last_flags) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ -1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_last_dist) <= 0 /\ -1 * (s IDinit_block_last_dist) <= 0)%Z
    | 22%positive => (-1 * (s IDinit_block_last_dist) <= 0 /\ 1 * (s IDinit_block_last_dist) <= 0 /\ -1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_last_flags) <= 0 /\ -1 * (s IDinit_block_last_flags) <= 0 /\ 1 * (s IDinit_block_last_lit) <= 0 /\ -1 * (s IDinit_block_last_lit) <= 0)%Z
    | 23%positive => (-1 * (s IDinit_block_last_lit) <= 0 /\ 1 * (s IDinit_block_last_lit) <= 0 /\ -1 * (s IDinit_block_last_flags) <= 0 /\ 1 * (s IDinit_block_last_flags) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ -1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_last_dist) <= 0 /\ -1 * (s IDinit_block_last_dist) <= 0 /\ 1 * (s IDinit_block_flags) <= 0 /\ -1 * (s IDinit_block_flags) <= 0)%Z
    | 24%positive => (-1 * (s IDinit_block_flags) <= 0 /\ 1 * (s IDinit_block_flags) <= 0 /\ -1 * (s IDinit_block_last_dist) <= 0 /\ 1 * (s IDinit_block_last_dist) <= 0 /\ -1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_last_flags) <= 0 /\ -1 * (s IDinit_block_last_flags) <= 0 /\ 1 * (s IDinit_block_last_lit) <= 0 /\ -1 * (s IDinit_block_last_lit) <= 0 /\ 1 * (s IDinit_block_flag_bit) + -1 <= 0 /\ -1 * (s IDinit_block_flag_bit) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDinit_block_flag_bit) + 1 <= 0 /\ 1 * (s IDinit_block_flag_bit) + -1 <= 0 /\ -1 * (s IDinit_block_last_lit) <= 0 /\ 1 * (s IDinit_block_last_lit) <= 0 /\ -1 * (s IDinit_block_last_flags) <= 0 /\ 1 * (s IDinit_block_last_flags) <= 0 /\ -1 * (s IDinit_block_static_len) <= 0 /\ 1 * (s IDinit_block_static_len) <= 0 /\ -1 * (s IDinit_block_n) + 19 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0 /\ 1 * (s IDinit_block_opt_len) <= 0 /\ -1 * (s IDinit_block_opt_len) <= 0 /\ 1 * (s IDinit_block_last_dist) <= 0 /\ -1 * (s IDinit_block_last_dist) <= 0 /\ 1 * (s IDinit_block_flags) <= 0 /\ -1 * (s IDinit_block_flags) <= 0)%Z
    | 26%positive => (-1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -18 <= 0)%Z
    | 27%positive => (1 * (s IDinit_block_n) + -18 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) <= 0)%Z
    | 28%positive => (-1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -18 <= 0)%Z
    | 29%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0)%Z
    | 30%positive => (1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 31%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ 1 * (s IDinit_block_n) + -19 <= 0)%Z
    | 32%positive => (1 * (s IDinit_block_n) + -19 <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ -1 * (s IDinit_block_z) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -29 <= 0)%Z
    | 34%positive => (1 * (s IDinit_block_n) + -29 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) <= 0)%Z
    | 35%positive => (-1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -29 <= 0)%Z
    | 36%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ 1 * (s IDinit_block_n) + -30 <= 0)%Z
    | 37%positive => (1 * (s IDinit_block_n) + -30 <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 38%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ 1 * (s IDinit_block_n) + -30 <= 0)%Z
    | 39%positive => (1 * (s IDinit_block_n) + -30 <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ -1 * (s IDinit_block_z) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -285 <= 0)%Z
    | 41%positive => (1 * (s IDinit_block_n) + -285 <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) <= 0)%Z
    | 42%positive => (-1 * (s IDinit_block_n) <= 0 /\ -1 * (s IDinit_block_z) <= 0 /\ 1 * (s IDinit_block_n) + -285 <= 0)%Z
    | 43%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ 1 * (s IDinit_block_n) + -286 <= 0)%Z
    | 44%positive => (1 * (s IDinit_block_n) + -286 <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ -1 * (s IDinit_block_z) <= 0)%Z
    | 45%positive => (-1 * (s IDinit_block_z) <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ 1 * (s IDinit_block_n) + -286 <= 0)%Z
    | 46%positive => (1 * (s IDinit_block_n) + -286 <= 0 /\ -1 * (s IDinit_block_n) + 1 <= 0 /\ -1 * (s IDinit_block_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition init_block_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((335 # 1))%Q
    | 2%positive => ((335 # 1) + (s IDinit_block_z))%Q
    | 3%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 4%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 5%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 6%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 7%positive => ((49 # 1) + (s IDinit_block_z))%Q
    | 8%positive => ((19 # 1) + (s IDinit_block_z)
                     + max0(30 - (s IDinit_block_n)))%Q
    | 9%positive => ((19 # 1) + (s IDinit_block_z)
                     + max0(30 - (s IDinit_block_n)))%Q
    | 10%positive => ((19 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 11%positive => ((19 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 12%positive => ((19 # 1) + (s IDinit_block_z))%Q
    | 13%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 14%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 15%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 16%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 17%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 18%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 19%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 20%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 21%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 22%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 23%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 24%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 25%positive => ((s IDinit_block_z))%Q
    | 26%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 27%positive => ((1 # 1) + (s IDinit_block_z)
                      + max0(18 - (s IDinit_block_n)))%Q
    | 28%positive => ((1 # 1) + (s IDinit_block_z)
                      + max0(18 - (s IDinit_block_n)))%Q
    | 29%positive => ((1 # 1) + (s IDinit_block_z)
                      + max0(19 - (s IDinit_block_n)))%Q
    | 30%positive => ((1 # 1) + (s IDinit_block_z)
                      + max0(19 - (s IDinit_block_n)))%Q
    | 31%positive => ((1 # 1) + (s IDinit_block_z)
                      + max0(19 - (s IDinit_block_n)))%Q
    | 32%positive => ((s IDinit_block_z) + max0(19 - (s IDinit_block_n)))%Q
    | 33%positive => ((19 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 34%positive => ((20 # 1) + (s IDinit_block_z)
                      + max0(29 - (s IDinit_block_n)))%Q
    | 35%positive => ((20 # 1) + (s IDinit_block_z)
                      + max0(29 - (s IDinit_block_n)))%Q
    | 36%positive => ((20 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 37%positive => ((20 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 38%positive => ((20 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 39%positive => ((19 # 1) + (s IDinit_block_z)
                      + max0(30 - (s IDinit_block_n)))%Q
    | 40%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 41%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 42%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 43%positive => ((336 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 44%positive => ((336 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 45%positive => ((336 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | 46%positive => ((335 # 1) - (s IDinit_block_n) + (s IDinit_block_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition init_block_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (286
                                                            - (s IDinit_block_n)) (285
                                                                    - (s IDinit_block_n)));
                     (*-1 0*) F_max0_ge_0 (285 - (s IDinit_block_n));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (286
                                                                    - (s IDinit_block_n)) (0))) (F_max0_ge_0 (286
                                                                    - (s IDinit_block_n)))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (30
                                                             - (s IDinit_block_n)) (29
                                                                    - (s IDinit_block_n)));
                      (*-1 0*) F_max0_ge_0 (29 - (s IDinit_block_n))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (19
                                                             - (s IDinit_block_n)) (18
                                                                    - (s IDinit_block_n)));
                      (*-1 0*) F_max0_ge_0 (18 - (s IDinit_block_n))]
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_pre_decrement (19 - (s IDinit_block_n)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*0 1*) F_max0_pre_decrement (30 - (s IDinit_block_n)) (1)]
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
    | _ => []
  end.


Theorem init_block_ai_correct:
  forall s p' s', steps (g_start init_block) s (g_edges init_block) p' s' -> init_block_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem init_block_pot_correct:
  forall s p' s',
    steps (g_start init_block) s (g_edges init_block) p' s' ->
    (init_block_pot (g_start init_block) s >= init_block_pot p' s')%Q.
Proof.
  check_lp init_block_ai_correct init_block_hints.
Qed.

