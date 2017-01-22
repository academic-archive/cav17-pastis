Require Import pasta.Pasta.

Notation IDct_tally_z := 1%positive.
Notation IDct_tally__tmp := 2%positive.
Notation IDct_tally__tmp1 := 3%positive.
Notation IDct_tally__tmp2 := 4%positive.
Notation IDct_tally_block_start := 5%positive.
Notation IDct_tally_dcode := 6%positive.
Notation IDct_tally_flag_bit := 7%positive.
Notation IDct_tally_flags := 8%positive.
Notation IDct_tally_in_length := 9%positive.
Notation IDct_tally_last_dist := 10%positive.
Notation IDct_tally_last_flags := 11%positive.
Notation IDct_tally_last_lit := 12%positive.
Notation IDct_tally_level := 13%positive.
Notation IDct_tally_out_length := 14%positive.
Notation IDct_tally_strstart := 15%positive.
Notation IDct_tally_dist := 16%positive.
Notation IDct_tally_lc := 17%positive.
Definition ct_tally : graph := {|
  g_start := 1%positive;
  g_end := 60%positive;
  g_edges := (1%positive,(AAssign IDct_tally_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDct_tally_out_length) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDct_tally_last_dist)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDct_tally__tmp
             (Some (EVar IDct_tally_dist))),6%positive)::
             (6%positive,(AAssign IDct_tally__tmp2
             (Some (EVar IDct_tally_lc))),7%positive)::
             (7%positive,(AAssign IDct_tally_last_lit
             (Some (EAdd (EVar IDct_tally_last_lit) (ENum (1))))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDct_tally__tmp) s) =
             (eval (ENum (0)) s))%Z)),21%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDct_tally__tmp)
             s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDct_tally__tmp
             (Some (EAdd (EVar IDct_tally__tmp) (ENum (-1))))),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDct_tally__tmp)
             s) < (eval (ENum (256)) s))%Z)),16%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDct_tally__tmp)
             s) >= (eval (ENum (256)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,18%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDct_tally_last_dist
             (Some (EAdd (EVar IDct_tally_last_dist) (ENum (1))))),
             19%positive)::
             (19%positive,(AAssign IDct_tally_flags None),20%positive)::
             (20%positive,ANone,23%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDct_tally_flag_bit None),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,27%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,32%positive)::
             (27%positive,(AAssign IDct_tally_last_flags
             (Some (EAdd (EVar IDct_tally_last_flags) (ENum (1))))),
             28%positive)::
             (28%positive,(AAssign IDct_tally_flags (Some (ENum (0)))),
             29%positive)::
             (29%positive,(AAssign IDct_tally_flag_bit (Some (ENum (1)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDct_tally_level)
             s) > (eval (ENum (2)) s))%Z)),34%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDct_tally_level)
             s) <= (eval (ENum (2)) s))%Z)),33%positive)::
             (33%positive,AWeaken,51%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,37%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,51%positive)::
             (37%positive,(AAssign IDct_tally_out_length
             (Some (EMul (EVar IDct_tally_last_lit) (ENum (8))))),
             38%positive)::
             (38%positive,(AAssign IDct_tally_in_length
             (Some (ESub (EVar IDct_tally_strstart)
             (EVar IDct_tally_block_start)))),39%positive)::
             (39%positive,(AAssign IDct_tally_dcode (Some (ENum (0)))),
             40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDct_tally_dcode)
             s) < (eval (ENum (30)) s))%Z)),61%positive)::
             (42%positive,(AGuard (fun s => ((eval (EVar IDct_tally_dcode)
             s) >= (eval (ENum (30)) s))%Z)),43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AAssign IDct_tally_out_length None),45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,47%positive)::
             (46%positive,ANone,49%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,57%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,56%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDct_tally__tmp1 None),54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,AWeaken,60%positive)::
             (56%positive,AWeaken,60%positive)::
             (57%positive,(AAssign IDct_tally__tmp1 (Some (ENum (1)))),
             58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,AWeaken,60%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AAssign IDct_tally_out_length None),63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDct_tally_dcode
             (Some (EAdd (EVar IDct_tally_dcode) (ENum (1))))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDct_tally_z (Some (EAdd (ENum (1))
             (EVar IDct_tally_z)))),68%positive)::
             (68%positive,AWeaken,42%positive)::nil
|}.

Definition ct_tally_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0)%Z
    | 3%positive => (-1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 4%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 5%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 6%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 7%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 8%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 9%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 10%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 11%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 12%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 13%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 14%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally__tmp) + 256 <= 0)%Z
    | 15%positive => (-1 * (s IDct_tally__tmp) + 256 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 16%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ 1 * (s IDct_tally__tmp) + -255 <= 0)%Z
    | 17%positive => (1 * (s IDct_tally__tmp) + -255 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 18%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 19%positive => (-1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ -1 * (s IDct_tally_last_dist) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDct_tally_last_dist) + 1 <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0)%Z
    | 21%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ 1 * (s IDct_tally__tmp) <= 0 /\ -1 * (s IDct_tally__tmp) <= 0)%Z
    | 22%positive => (-1 * (s IDct_tally__tmp) <= 0 /\ 1 * (s IDct_tally__tmp) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 23%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 24%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 25%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 26%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 27%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0)%Z
    | 28%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 29%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_flags) <= 0 /\ -1 * (s IDct_tally_flags) <= 0)%Z
    | 30%positive => (-1 * (s IDct_tally_flags) <= 0 /\ 1 * (s IDct_tally_flags) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ 1 * (s IDct_tally_flag_bit) + -1 <= 0 /\ -1 * (s IDct_tally_flag_bit) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDct_tally_flag_bit) + 1 <= 0 /\ 1 * (s IDct_tally_flag_bit) + -1 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_flags) <= 0 /\ -1 * (s IDct_tally_flags) <= 0)%Z
    | 32%positive => (-1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 33%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_level) + -2 <= 0)%Z
    | 34%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 35%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 36%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 37%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_out_length) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 38%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 39%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 40%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ 1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_dcode) <= 0)%Z
    | 41%positive => (-1 * (s IDct_tally_dcode) <= 0 /\ 1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 42%positive => (-1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0)%Z
    | 43%positive => (1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) + 30 <= 0)%Z
    | 44%positive => (-1 * (s IDct_tally_dcode) + 30 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0)%Z
    | 45%positive => (1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) + 30 <= 0)%Z
    | 46%positive => (-1 * (s IDct_tally_dcode) + 30 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0)%Z
    | 47%positive => (1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) + 30 <= 0)%Z
    | 48%positive => (-1 * (s IDct_tally_dcode) + 30 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0)%Z
    | 49%positive => (1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) + 30 <= 0)%Z
    | 50%positive => (-1 * (s IDct_tally_dcode) + 30 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0)%Z
    | 51%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0)%Z
    | 52%positive => (-1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 53%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0)%Z
    | 54%positive => (-1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 55%positive => (-1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0)%Z
    | 56%positive => (-1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 57%positive => (1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) + 30 <= 0)%Z
    | 58%positive => (-1 * (s IDct_tally_dcode) + 30 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0 /\ 1 * (s IDct_tally__tmp1) + -1 <= 0 /\ -1 * (s IDct_tally__tmp1) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDct_tally__tmp1) + 1 <= 0 /\ 1 * (s IDct_tally__tmp1) + -1 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) + 30 <= 0)%Z
    | 60%positive => (-1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0)%Z
    | 61%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_dcode) + -29 <= 0)%Z
    | 62%positive => (1 * (s IDct_tally_dcode) + -29 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 63%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_dcode) + -29 <= 0)%Z
    | 64%positive => (1 * (s IDct_tally_dcode) + -29 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_dcode) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 65%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_dcode) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDct_tally_dcode) + 1 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0)%Z
    | 67%positive => (-1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_z) <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_dcode) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDct_tally_dcode) + 1 <= 0 /\ 1 * (s IDct_tally_dcode) + -30 <= 0 /\ -1 * (s IDct_tally_last_dist) <= 0 /\ -1 * (s IDct_tally_level) + 3 <= 0 /\ -1 * (s IDct_tally_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ct_tally_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((30 # 1))%Q
    | 2%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 3%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 4%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 5%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 6%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 7%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 8%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 9%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 10%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 11%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 12%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 13%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 14%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 15%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 16%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 17%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 18%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 19%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 20%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 21%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 22%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 23%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 24%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 25%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 26%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 27%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 28%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 29%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 30%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 31%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 32%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 33%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 34%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 35%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 36%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 37%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 38%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 39%positive => ((30 # 1) + (s IDct_tally_z))%Q
    | 40%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 41%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 42%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 43%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 44%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 45%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 46%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 47%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 48%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 49%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 50%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 51%positive => ((s IDct_tally_z))%Q
    | 52%positive => ((s IDct_tally_z))%Q
    | 53%positive => ((s IDct_tally_z))%Q
    | 54%positive => ((s IDct_tally_z))%Q
    | 55%positive => ((s IDct_tally_z))%Q
    | 56%positive => ((s IDct_tally_z))%Q
    | 57%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 58%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 59%positive => ((s IDct_tally_z) + max0(29 - (s IDct_tally_dcode)))%Q
    | 60%positive => ((s IDct_tally_z))%Q
    | 61%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | 62%positive => ((1 # 1) + (s IDct_tally_z)
                      + max0(29 - (s IDct_tally_dcode)))%Q
    | 63%positive => ((1 # 1) + (s IDct_tally_z)
                      + max0(29 - (s IDct_tally_dcode)))%Q
    | 64%positive => ((1 # 1) + (s IDct_tally_z)
                      + max0(29 - (s IDct_tally_dcode)))%Q
    | 65%positive => ((1 # 1) + (s IDct_tally_z)
                      + max0(30 - (s IDct_tally_dcode)))%Q
    | 66%positive => ((1 # 1) + (s IDct_tally_z)
                      + max0(30 - (s IDct_tally_dcode)))%Q
    | 67%positive => ((1 # 1) + (s IDct_tally_z)
                      + max0(30 - (s IDct_tally_dcode)))%Q
    | 68%positive => ((s IDct_tally_z) + max0(30 - (s IDct_tally_dcode)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ct_tally_hints (p : node) (s : state) := 
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
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-30 0*) F_one]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-30 0*) F_one]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (30
                                                             - (s IDct_tally_dcode)) (29
                                                                    - (s IDct_tally_dcode)))]
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_max0_ge_0 (29 - (s IDct_tally_dcode))]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*-1 0*) F_max0_ge_0 (29 - (s IDct_tally_dcode))]
    | 60%positive => []
    | 61%positive => [(*-1 0*) F_max0_pre_decrement (30
                                                     - (s IDct_tally_dcode)) (1)]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | _ => []
  end.


Theorem ct_tally_ai_correct:
  forall s p' s', steps (g_start ct_tally) s (g_edges ct_tally) p' s' -> ct_tally_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ct_tally_pot_correct:
  forall s p' s',
    steps (g_start ct_tally) s (g_edges ct_tally) p' s' ->
    (ct_tally_pot (g_start ct_tally) s >= ct_tally_pot p' s')%Q.
Proof.
  check_lp ct_tally_ai_correct ct_tally_hints.
Qed.

