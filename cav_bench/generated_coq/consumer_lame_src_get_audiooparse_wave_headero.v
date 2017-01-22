Require Import pasta.Pasta.

Notation IDparse_wave_header_z := 1%positive.
Notation IDparse_wave_header__tmp := 2%positive.
Notation IDparse_wave_header_data_length := 3%positive.
Notation IDparse_wave_header_file_length := 4%positive.
Notation IDparse_wave_header_is_wav := 5%positive.
Notation IDparse_wave_header_loop_sanity := 6%positive.
Notation IDparse_wave_header_num_channels := 7%positive.
Notation IDparse_wave_header_num_samples := 8%positive.
Notation IDparse_wave_header_samp_freq := 9%positive.
Notation IDparse_wave_header_subSize := 10%positive.
Notation IDparse_wave_header_type := 11%positive.
Notation IDparse_wave_header_sf := 12%positive.
Definition parse_wave_header : graph := {|
  g_start := 1%positive;
  g_end := 76%positive;
  g_edges := (1%positive,(AAssign IDparse_wave_header_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDparse_wave_header_is_wav
             (Some (ENum (0)))),3%positive)::
             (3%positive,(AAssign IDparse_wave_header_data_length
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDparse_wave_header_subSize
             (Some (ENum (0)))),5%positive)::
             (5%positive,(AAssign IDparse_wave_header_loop_sanity
             (Some (ENum (0)))),6%positive)::
             (6%positive,(AAssign IDparse_wave_header_file_length None),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,ANone,73%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDparse_wave_header_loop_sanity
             (Some (ENum (0)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_loop_sanity) s) <
             (eval (ENum (20)) s))%Z)),14%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_loop_sanity) s) >=
             (eval (ENum (20)) s))%Z)),13%positive)::
             (13%positive,AWeaken,33%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDparse_wave_header_type None),
             16%positive)::(16%positive,AWeaken,17%positive)::
             (17%positive,ANone,43%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,28%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDparse_wave_header_subSize None),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,ANone,25%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,60%positive)::
             (25%positive,(AAssign IDparse_wave_header__tmp
             (Some (ENum (0)))),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,76%positive)::
             (28%positive,(AAssign IDparse_wave_header_subSize None),
             29%positive)::
             (29%positive,(AAssign IDparse_wave_header_data_length
             (Some (EVar IDparse_wave_header_subSize))),30%positive)::
             (30%positive,(AAssign IDparse_wave_header_is_wav
             (Some (ENum (1)))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_is_wav) s) <>
             (eval (ENum (0)) s))%Z)),35%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_is_wav) s) =
             (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,40%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDparse_wave_header_num_channels None),
             37%positive)::
             (37%positive,(AAssign IDparse_wave_header_samp_freq None),
             38%positive)::
             (38%positive,(AAssign IDparse_wave_header_num_samples None),
             39%positive)::(39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDparse_wave_header__tmp
             (Some (EVar IDparse_wave_header_is_wav))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,76%positive)::
             (43%positive,(AAssign IDparse_wave_header_subSize None),
             44%positive)::(44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_subSize) s) <
             (eval (ENum (16)) s))%Z)),69%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_subSize) s) >=
             (eval (ENum (16)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AAssign IDparse_wave_header_subSize
             (Some (ESub (EVar IDparse_wave_header_subSize) (ENum (2))))),
             48%positive)::
             (48%positive,(AAssign IDparse_wave_header_subSize
             (Some (ESub (EVar IDparse_wave_header_subSize) (ENum (2))))),
             49%positive)::
             (49%positive,(AAssign IDparse_wave_header_subSize
             (Some (ESub (EVar IDparse_wave_header_subSize) (ENum (4))))),
             50%positive)::
             (50%positive,(AAssign IDparse_wave_header_subSize
             (Some (ESub (EVar IDparse_wave_header_subSize) (ENum (4))))),
             51%positive)::
             (51%positive,(AAssign IDparse_wave_header_subSize
             (Some (ESub (EVar IDparse_wave_header_subSize) (ENum (2))))),
             52%positive)::
             (52%positive,(AAssign IDparse_wave_header_subSize
             (Some (ESub (EVar IDparse_wave_header_subSize) (ENum (2))))),
             53%positive)::(53%positive,AWeaken,54%positive)::
             (54%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_subSize) s) >
             (eval (ENum (0)) s))%Z)),56%positive)::
             (54%positive,(AGuard
             (fun s => ((eval (EVar IDparse_wave_header_subSize) s) <=
             (eval (ENum (0)) s))%Z)),55%positive)::
             (55%positive,AWeaken,59%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,ANone,66%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,(AAssign IDparse_wave_header_loop_sanity
             (Some (EAdd (EVar IDparse_wave_header_loop_sanity)
             (ENum (1))))),62%positive)::(62%positive,ANone,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,(AAssign IDparse_wave_header_z
             (Some (EAdd (ENum (1)) (EVar IDparse_wave_header_z)))),
             65%positive)::(65%positive,AWeaken,12%positive)::
             (66%positive,(AAssign IDparse_wave_header__tmp
             (Some (ENum (0)))),67%positive)::
             (67%positive,ANone,68%positive)::
             (68%positive,AWeaken,76%positive)::
             (69%positive,AWeaken,70%positive)::
             (70%positive,(AAssign IDparse_wave_header__tmp
             (Some (ENum (0)))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,76%positive)::
             (73%positive,(AAssign IDparse_wave_header__tmp
             (Some (ENum (0)))),74%positive)::
             (74%positive,ANone,75%positive)::
             (75%positive,AWeaken,76%positive)::nil
|}.

Definition parse_wave_header_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0)%Z
    | 3%positive => (-1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 4%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0)%Z
    | 5%positive => (-1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 6%positive => (-1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 7%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 8%positive => (-1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 9%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 10%positive => (-1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 11%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 12%positive => (-1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0)%Z
    | 13%positive => (1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) + 20 <= 0)%Z
    | 14%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 15%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 16%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 17%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 18%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 19%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 20%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 21%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 22%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 23%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 24%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 25%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 26%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ -1 * (s IDparse_wave_header__tmp) <= 0)%Z
    | 27%positive => (-1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 28%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 29%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 30%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 31%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDparse_wave_header_is_wav) + 1 <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 33%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0)%Z
    | 34%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 35%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) + 1 <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0)%Z
    | 36%positive => (1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) + 1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 37%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) + 1 <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0)%Z
    | 38%positive => (1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) + 1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 39%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) + 1 <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0)%Z
    | 40%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 41%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header__tmp) + -1 <= 0 /\ -1 * (s IDparse_wave_header__tmp) <= 0)%Z
    | 42%positive => (-1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header__tmp) + -1 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0)%Z
    | 43%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 44%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 45%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 46%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 16 <= 0)%Z
    | 47%positive => (-1 * (s IDparse_wave_header_subSize) + 16 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 48%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 14 <= 0)%Z
    | 49%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 12 <= 0)%Z
    | 50%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 8 <= 0)%Z
    | 51%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 4 <= 0)%Z
    | 52%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 2 <= 0)%Z
    | 53%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 54%positive => (-1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 55%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 56%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 1 <= 0)%Z
    | 57%positive => (-1 * (s IDparse_wave_header_subSize) + 1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 58%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 60%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 61%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 62%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) + 1 <= 0)%Z
    | 63%positive => (-1 * (s IDparse_wave_header_loop_sanity) + 1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0)%Z
    | 64%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDparse_wave_header_loop_sanity) + 1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_z) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDparse_wave_header_subSize) + 1 <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ -1 * (s IDparse_wave_header__tmp) <= 0)%Z
    | 68%positive => (-1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_subSize) + 1 <= 0)%Z
    | 69%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) + -15 <= 0)%Z
    | 70%positive => (1 * (s IDparse_wave_header_subSize) + -15 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 71%positive => (1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) + -15 <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ -1 * (s IDparse_wave_header__tmp) <= 0)%Z
    | 72%positive => (-1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header_subSize) + -15 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) + -19 <= 0)%Z
    | 73%positive => (-1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 74%positive => (-1 * (s IDparse_wave_header_subSize) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ -1 * (s IDparse_wave_header__tmp) <= 0)%Z
    | 75%positive => (-1 * (s IDparse_wave_header__tmp) <= 0 /\ 1 * (s IDparse_wave_header__tmp) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ 1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header_data_length) <= 0 /\ 1 * (s IDparse_wave_header_data_length) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_z) <= 0 /\ 1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ 1 * (s IDparse_wave_header_subSize) <= 0 /\ -1 * (s IDparse_wave_header_subSize) <= 0)%Z
    | 76%positive => (1 * (s IDparse_wave_header_loop_sanity) + -20 <= 0 /\ 1 * (s IDparse_wave_header_is_wav) + -1 <= 0 /\ 1 * (s IDparse_wave_header__tmp) + -1 <= 0 /\ -1 * (s IDparse_wave_header_is_wav) <= 0 /\ -1 * (s IDparse_wave_header_z) <= 0 /\ -1 * (s IDparse_wave_header_loop_sanity) <= 0 /\ -1 * (s IDparse_wave_header__tmp) <= 0)%Z
    | _ => False
  end.

Definition parse_wave_header_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((20 # 1))%Q
    | 2%positive => ((20 # 1) + (s IDparse_wave_header_z))%Q
    | 3%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 4%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 5%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 6%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 7%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 8%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 9%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                     + (s IDparse_wave_header_z)
                     + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 10%positive => (-(560 # 19) + (20 # 1) * (s IDparse_wave_header_is_wav)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav))
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 11%positive => (-(560 # 19) + (20 # 1) * (s IDparse_wave_header_is_wav)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav))
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 12%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 13%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 14%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 15%positive => ((371 # 19) - (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 16%positive => ((371 # 19) - (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + (9 # 19) * max0(20
                                        - (s IDparse_wave_header_loop_sanity)))%Q
    | 17%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 18%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 19%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 20%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 21%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 22%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 23%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 24%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 25%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 26%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 27%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 28%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 29%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 30%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 31%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 32%positive => ((10 # 1)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity))
                      + (1 # 19) * max0((s IDparse_wave_header_loop_sanity)))%Q
    | 33%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 34%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 35%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 36%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 37%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 38%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 39%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 40%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 41%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 42%positive => ((200 # 19)
                      - (10 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 43%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 44%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 45%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 46%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 47%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 48%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 49%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 50%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 51%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 52%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 53%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 54%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 55%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 56%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 57%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 58%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 59%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 60%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 61%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 62%positive => ((199 # 19)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 63%positive => ((199 # 19)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 64%positive => ((199 # 19)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 65%positive => ((180 # 19)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(20
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 66%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 67%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 68%positive => ((10 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      + (10 # 19) * max0(19
                                         - (s IDparse_wave_header_loop_sanity)))%Q
    | 69%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 70%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 71%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 72%positive => ((9 # 1)
                      - (9 # 19) * (s IDparse_wave_header_loop_sanity)
                      + (s IDparse_wave_header_z)
                      - (9 # 19) * max0(19
                                        - (s IDparse_wave_header_loop_sanity))
                      + max0(20 - (s IDparse_wave_header_loop_sanity)))%Q
    | 73%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                      + (s IDparse_wave_header_z)
                      + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 74%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                      + (s IDparse_wave_header_z)
                      + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 75%positive => (-(20 # 1) + (20 # 1) * (s IDparse_wave_header_is_wav)
                      + (s IDparse_wave_header_z)
                      + (40 # 1) * max0(1 - (s IDparse_wave_header_is_wav)))%Q
    | 76%positive => ((s IDparse_wave_header_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition parse_wave_header_hints (p : node) (s : state) := 
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
    | 11%positive => [(*0 20*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDparse_wave_header_is_wav))) (F_check_ge (0) (0));
                      (*-20 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDparse_wave_header_is_wav)) (0))) (F_max0_ge_0 (-
                                                                    (s IDparse_wave_header_is_wav)));
                      (*-40 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    (s IDparse_wave_header_is_wav))) (F_check_ge (1
                                                                    - (s IDparse_wave_header_is_wav)) (0))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (20
                                                             - (s IDparse_wave_header_loop_sanity)) (19
                                                                    - (s IDparse_wave_header_loop_sanity)));
                      (*-1 0*) F_max0_ge_0 (19
                                            - (s IDparse_wave_header_loop_sanity))]
    | 14%positive => [(*0 0.473684*) F_binom_monotonic 1 (F_max0_ge_arg (19
                                                                    - (s IDparse_wave_header_loop_sanity))) (F_check_ge (19
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))]
    | 15%positive => []
    | 16%positive => [(*-0.526316 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDparse_wave_header_loop_sanity)))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_pre_decrement (20
                                                     - (s IDparse_wave_header_loop_sanity)) (1);
                      (*0 0.0526316*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_wave_header_loop_sanity)) (0))) (F_max0_ge_0 ((s IDparse_wave_header_loop_sanity)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_wave_header_loop_sanity))) (F_check_ge ((s IDparse_wave_header_loop_sanity)) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_one;
                      (*-0.473684 0*) F_max0_pre_decrement (20
                                                            - (s IDparse_wave_header_loop_sanity)) (1);
                      (*-1 0*) F_max0_ge_0 (19
                                            - (s IDparse_wave_header_loop_sanity));
                      (*-0.473684 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDparse_wave_header_loop_sanity)))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-0.526316 0*) F_max0_pre_decrement (20
                                                            - (s IDparse_wave_header_loop_sanity)) (1);
                      (*-1.05263 0*) F_max0_ge_0 (19
                                                  - (s IDparse_wave_header_loop_sanity));
                      (*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDparse_wave_header_loop_sanity))) (F_check_ge (0) (0))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-0.526316 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDparse_wave_header_loop_sanity)))]
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
    | 55%positive => [(*-1 0*) F_max0_pre_decrement (20
                                                     - (s IDparse_wave_header_loop_sanity)) (1)]
    | 56%positive => [(*-1 0*) F_max0_pre_decrement (20
                                                     - (s IDparse_wave_header_loop_sanity)) (1)]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => [(*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_arg (20
                                                                    - (s IDparse_wave_header_loop_sanity))) (F_check_ge (20
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))]
    | 66%positive => []
    | 67%positive => []
    | 68%positive => [(*-1 0*) F_one;
                      (*-0.473684 0*) F_max0_pre_decrement (20
                                                            - (s IDparse_wave_header_loop_sanity)) (1);
                      (*-1 0*) F_max0_ge_0 (19
                                            - (s IDparse_wave_header_loop_sanity));
                      (*-0.473684 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDparse_wave_header_loop_sanity)))]
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (20
                                                             - (s IDparse_wave_header_loop_sanity)) (19
                                                                    - (s IDparse_wave_header_loop_sanity)));
                      (*-1 0*) F_max0_ge_0 (19
                                            - (s IDparse_wave_header_loop_sanity));
                      (*-0.473684 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (19
                                                                    - (s IDparse_wave_header_loop_sanity)) (0))) (F_max0_ge_0 (19
                                                                    - (s IDparse_wave_header_loop_sanity)))]
    | 73%positive => []
    | 74%positive => []
    | 75%positive => [(*-20 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                  - (s IDparse_wave_header_is_wav))) (F_check_ge (0) (0));
                      (*-20 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    (s IDparse_wave_header_is_wav))) (F_check_ge (1
                                                                    - (s IDparse_wave_header_is_wav)) (0))]
    | 76%positive => []
    | _ => []
  end.


Theorem parse_wave_header_ai_correct:
  forall s p' s', steps (g_start parse_wave_header) s (g_edges parse_wave_header) p' s' -> parse_wave_header_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem parse_wave_header_pot_correct:
  forall s p' s',
    steps (g_start parse_wave_header) s (g_edges parse_wave_header) p' s' ->
    (parse_wave_header_pot (g_start parse_wave_header) s >= parse_wave_header_pot p' s')%Q.
Proof.
  check_lp parse_wave_header_ai_correct parse_wave_header_hints.
Qed.

