Require Import pasta.Pasta.

Notation IDBF_set_key_z := 1%positive.
Notation IDBF_set_key__tmp := 2%positive.
Notation IDBF_set_key_i := 3%positive.
Notation IDBF_set_key_ri := 4%positive.
Notation IDBF_set_key_data := 5%positive.
Notation IDBF_set_key_key := 6%positive.
Notation IDBF_set_key_len := 7%positive.
Definition BF_set_key : graph := {|
  g_start := 1%positive;
  g_end := 24%positive;
  g_edges := (1%positive,(AAssign IDBF_set_key_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDBF_set_key__tmp
             (Some (EVar IDBF_set_key_len))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key__tmp)
             s) > (eval (ENum (72)) s))%Z)),6%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key__tmp)
             s) <= (eval (ENum (72)) s))%Z)),5%positive)::
             (5%positive,AWeaken,9%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDBF_set_key__tmp (Some (ENum (72)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDBF_set_key_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key_i) s) <
             (eval (ENum (18)) s))%Z)),39%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key_i)
             s) >= (eval (ENum (18)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDBF_set_key_i (Some (ENum (0)))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key_i) s) <
             (eval (ENum (18)) s))%Z)),32%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key_i)
             s) >= (eval (ENum (18)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDBF_set_key_i (Some (ENum (0)))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key_i) s) <
             (eval (ENum (1024)) s))%Z)),25%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDBF_set_key_i)
             s) >= (eval (ENum (1024)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDBF_set_key_i
             (Some (EAdd (EVar IDBF_set_key_i) (ENum (2))))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDBF_set_key_z (Some (EAdd (ENum (1))
             (EVar IDBF_set_key_z)))),31%positive)::
             (31%positive,AWeaken,22%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDBF_set_key_i
             (Some (EAdd (EVar IDBF_set_key_i) (ENum (2))))),35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDBF_set_key_z (Some (EAdd (ENum (1))
             (EVar IDBF_set_key_z)))),38%positive)::
             (38%positive,AWeaken,17%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDBF_set_key_ri None),41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (42%positive,ANone,44%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDBF_set_key_ri None),45%positive)::
             (45%positive,(AAssign IDBF_set_key_ri None),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,48%positive)::
             (47%positive,ANone,49%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDBF_set_key_ri None),50%positive)::
             (50%positive,(AAssign IDBF_set_key_ri None),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (52%positive,ANone,54%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDBF_set_key_ri None),55%positive)::
             (55%positive,(AAssign IDBF_set_key_ri None),56%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,58%positive)::
             (57%positive,ANone,59%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDBF_set_key_i
             (Some (EAdd (EVar IDBF_set_key_i) (ENum (1))))),61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,(AAssign IDBF_set_key_z (Some (EAdd (ENum (1))
             (EVar IDBF_set_key_z)))),64%positive)::
             (64%positive,AWeaken,12%positive)::nil
|}.

Definition BF_set_key_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_z) <= 0)%Z
    | 4%positive => (1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 5%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 6%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key__tmp) + 73 <= 0)%Z
    | 7%positive => (-1 * (s IDBF_set_key__tmp) + 73 <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 8%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key__tmp) + 72 <= 0)%Z
    | 9%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 10%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0)%Z
    | 11%positive => (-1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 12%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) + -18 <= 0)%Z
    | 13%positive => (1 * (s IDBF_set_key_i) + -18 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) + 18 <= 0)%Z
    | 14%positive => (-1 * (s IDBF_set_key_i) + 18 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) + -18 <= 0)%Z
    | 15%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0)%Z
    | 16%positive => (-1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 17%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) + -19 <= 0)%Z
    | 18%positive => (1 * (s IDBF_set_key_i) + -19 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) + 18 <= 0)%Z
    | 19%positive => (-1 * (s IDBF_set_key_i) + 18 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) + -19 <= 0)%Z
    | 20%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0)%Z
    | 21%positive => (-1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 22%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) + -1025 <= 0)%Z
    | 23%positive => (1 * (s IDBF_set_key_i) + -1025 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) + 1024 <= 0)%Z
    | 24%positive => (-1 * (s IDBF_set_key_i) + 1024 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ 1 * (s IDBF_set_key_i) + -1025 <= 0)%Z
    | 25%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -1023 <= 0)%Z
    | 26%positive => (1 * (s IDBF_set_key_i) + -1023 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 27%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -1023 <= 0)%Z
    | 28%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key_i) + -1025 <= 0)%Z
    | 29%positive => (1 * (s IDBF_set_key_i) + -1025 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 30%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key_i) + -1025 <= 0)%Z
    | 31%positive => (1 * (s IDBF_set_key_i) + -1025 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 33%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 34%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 35%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key_i) + -19 <= 0)%Z
    | 36%positive => (1 * (s IDBF_set_key_i) + -19 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0)%Z
    | 37%positive => (-1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key_i) + -19 <= 0)%Z
    | 38%positive => (1 * (s IDBF_set_key_i) + -19 <= 0 /\ -1 * (s IDBF_set_key_i) + 2 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) + 1 <= 0)%Z
    | 39%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 40%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 41%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 42%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 43%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 44%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 45%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 46%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 47%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 48%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 49%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 50%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 51%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 52%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 53%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 54%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 55%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 56%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 57%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 58%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 59%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -17 <= 0)%Z
    | 60%positive => (1 * (s IDBF_set_key_i) + -17 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ -1 * (s IDBF_set_key_i) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 61%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -18 <= 0 /\ -1 * (s IDBF_set_key_i) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDBF_set_key_i) + 1 <= 0 /\ 1 * (s IDBF_set_key_i) + -18 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0)%Z
    | 63%positive => (1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) <= 0 /\ 1 * (s IDBF_set_key_i) + -18 <= 0 /\ -1 * (s IDBF_set_key_i) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDBF_set_key_i) + 1 <= 0 /\ 1 * (s IDBF_set_key_i) + -18 <= 0 /\ 1 * (s IDBF_set_key__tmp) + -72 <= 0 /\ -1 * (s IDBF_set_key_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition BF_set_key_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((540 # 1))%Q
    | 2%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 3%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 4%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 5%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 6%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 7%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 8%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 9%positive => ((540 # 1) + (s IDBF_set_key_z))%Q
    | 10%positive => ((522 # 1) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i)))%Q
    | 11%positive => ((522 # 1) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i)))%Q
    | 12%positive => ((522 # 1) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i)))%Q
    | 13%positive => ((522 # 1) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i)))%Q
    | 14%positive => (-(17 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 15%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 16%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 17%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 18%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 19%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 20%positive => (-(1061 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 21%positive => (-(1061 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 22%positive => (-(1061 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 23%positive => (-(1061 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 24%positive => ((s IDBF_set_key_z))%Q
    | 25%positive => (-(1061 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 26%positive => (-(1059 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1023 - (s IDBF_set_key_i)))%Q
    | 27%positive => (-(1059 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1023 - (s IDBF_set_key_i)))%Q
    | 28%positive => (-(1059 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 29%positive => (-(1059 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 30%positive => (-(1059 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 31%positive => (-(1061 # 2) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp))
                      + (1 # 2) * max0(1025 - (s IDBF_set_key_i)))%Q
    | 32%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 33%positive => (-(17 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(17 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 34%positive => (-(17 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(17 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 35%positive => (-(17 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 36%positive => (-(17 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 37%positive => (-(17 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 38%positive => (-(18 # 1) + (921 # 125) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z)
                      + (1 # 2) * max0(19 - (s IDBF_set_key_i))
                      + (921 # 125) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 39%positive => ((522 # 1) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i)))%Q
    | 40%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 41%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 42%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 43%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 44%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 45%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 46%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 47%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 48%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 49%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 50%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 51%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 52%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 53%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 54%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 55%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 56%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 57%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 58%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 59%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 60%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(18 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 61%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(19 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 62%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(19 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 63%positive => ((29 # 4) * (s IDBF_set_key__tmp) + (s IDBF_set_key_z)
                      + max0(19 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | 64%positive => (-(1 # 1) + (29 # 4) * (s IDBF_set_key__tmp)
                      + (s IDBF_set_key_z) + max0(19 - (s IDBF_set_key_i))
                      + (29 # 4) * max0(72 - (s IDBF_set_key__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition BF_set_key_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (18
                                                             - (s IDBF_set_key_i)) (17
                                                                    - (s IDBF_set_key_i)));
                      (*-1 0*) F_max0_ge_0 (17 - (s IDBF_set_key_i));
                      (*0 7.36806*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (72
                                                                    - (s IDBF_set_key__tmp)) (0))) (F_max0_ge_0 (72
                                                                    - (s IDBF_set_key__tmp)))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (18
                                                               - (s IDBF_set_key_i)) (17
                                                                    - (s IDBF_set_key_i)));
                      (*-0.5 0*) F_max0_ge_0 (17 - (s IDBF_set_key_i));
                      (*-0.5 0*) F_max0_monotonic (F_check_ge (19
                                                               - (s IDBF_set_key_i)) (18
                                                                    - (s IDBF_set_key_i)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1025
                                                               - (s IDBF_set_key_i)) (1024
                                                                    - (s IDBF_set_key_i)));
                      (*-0.5 0*) F_max0_ge_0 (1024 - (s IDBF_set_key_i));
                      (*-7.36806 0*) F_binom_monotonic 1 (F_max0_ge_arg (72
                                                                    - (s IDBF_set_key__tmp))) (F_check_ge (72
                                                                    - (s IDBF_set_key__tmp)) (0))]
    | 24%positive => []
    | 25%positive => [(*-0.5 0*) F_max0_pre_decrement (1025
                                                       - (s IDBF_set_key_i)) (2)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-0.5 0*) F_max0_pre_decrement (19
                                                       - (s IDBF_set_key_i)) (2)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*0 7.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (72
                                                                    - (s IDBF_set_key__tmp)) (0))) (F_max0_ge_0 (72
                                                                    - (s IDBF_set_key__tmp)))]
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
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => [(*-1 0*) F_max0_pre_decrement (19 - (s IDBF_set_key_i)) (1);
                      (*-7.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (72
                                                                    - (s IDBF_set_key__tmp))) (F_check_ge (72
                                                                    - (s IDBF_set_key__tmp)) (0))]
    | _ => []
  end.


Theorem BF_set_key_ai_correct:
  forall s p' s', steps (g_start BF_set_key) s (g_edges BF_set_key) p' s' -> BF_set_key_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BF_set_key_pot_correct:
  forall s p' s',
    steps (g_start BF_set_key) s (g_edges BF_set_key) p' s' ->
    (BF_set_key_pot (g_start BF_set_key) s >= BF_set_key_pot p' s')%Q.
Proof.
  check_lp BF_set_key_ai_correct BF_set_key_hints.
Qed.

