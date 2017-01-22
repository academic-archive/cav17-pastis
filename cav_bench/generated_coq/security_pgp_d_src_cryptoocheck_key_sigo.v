Require Import pasta.Pasta.

Notation IDcheck_key_sig_z := 1%positive.
Notation IDcheck_key_sig__tmp := 2%positive.
Notation IDcheck_key_sig__tmp1 := 3%positive.
Notation IDcheck_key_sig__tmp2 := 4%positive.
Notation IDcheck_key_sig__tmp3 := 5%positive.
Notation IDcheck_key_sig_algorithm := 6%positive.
Notation IDcheck_key_sig_cert_length := 7%positive.
Notation IDcheck_key_sig_class := 8%positive.
Notation IDcheck_key_sig_count := 9%positive.
Notation IDcheck_key_sig_global_precision := 10%positive.
Notation IDcheck_key_sig_i := 11%positive.
Notation IDcheck_key_sig_mdlen := 12%positive.
Notation IDcheck_key_sig_mdlow2_off0 := 13%positive.
Notation IDcheck_key_sig_mdlow2_off1 := 14%positive.
Notation IDcheck_key_sig_sigclass_dref := 15%positive.
Notation IDcheck_key_sig_version := 16%positive.
Notation IDcheck_key_sig_fkey := 17%positive.
Notation IDcheck_key_sig_fpkey := 18%positive.
Notation IDcheck_key_sig_fpsig := 19%positive.
Notation IDcheck_key_sig_fsig := 20%positive.
Notation IDcheck_key_sig_keyfile := 21%positive.
Notation IDcheck_key_sig_keypktlen := 22%positive.
Notation IDcheck_key_sig_keyuserid := 23%positive.
Notation IDcheck_key_sig_sigclass := 24%positive.
Notation IDcheck_key_sig_siguserid := 25%positive.
Notation IDcheck_key_sig_xtimestamp := 26%positive.
Definition check_key_sig : graph := {|
  g_start := 1%positive;
  g_end := 143%positive;
  g_edges := (1%positive,(AAssign IDcheck_key_sig_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_cert_length) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcheck_key_sig__tmp3
             (Some (EVar IDcheck_key_sig_fpkey))),5%positive)::
             (5%positive,(AAssign IDcheck_key_sig__tmp2
             (Some (EVar IDcheck_key_sig_keypktlen))),6%positive)::
             (6%positive,(AAssign IDcheck_key_sig__tmp1
             (Some (EVar IDcheck_key_sig_fpsig))),7%positive)::
             (7%positive,(AAssign IDcheck_key_sig_global_precision
             (Some (ENum (130)))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::(9%positive,ANone,12%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,13%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,140%positive)::
             (13%positive,(AAssign IDcheck_key_sig_cert_length None),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_cert_length) s) >
             (eval (ENum (283)) s))%Z)),138%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_cert_length) s) <=
             (eval (ENum (283)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,137%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDcheck_key_sig_version None),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,134%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDcheck_key_sig_mdlen None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,131%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDcheck_key_sig_class None),25%positive)::
             (25%positive,(AAssign IDcheck_key_sig_sigclass_dref None),
             26%positive)::(26%positive,AWeaken,27%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) <>
             (eval (ENum (16)) s))%Z)),29%positive)::
             (27%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) =
             (eval (ENum (16)) s))%Z)),28%positive)::
             (28%positive,AWeaken,41%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) <>
             (eval (ENum (17)) s))%Z)),32%positive)::
             (30%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) =
             (eval (ENum (17)) s))%Z)),31%positive)::
             (31%positive,AWeaken,41%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) <>
             (eval (ENum (18)) s))%Z)),35%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) =
             (eval (ENum (18)) s))%Z)),34%positive)::
             (34%positive,AWeaken,41%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) <>
             (eval (ENum (19)) s))%Z)),38%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) =
             (eval (ENum (19)) s))%Z)),37%positive)::
             (37%positive,AWeaken,41%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) <>
             (eval (ENum (32)) s))%Z)),127%positive)::
             (39%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) =
             (eval (ENum (32)) s))%Z)),40%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,(AAssign IDcheck_key_sig_i (Some (ENum (0)))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AGuard (fun s => ((eval (EVar IDcheck_key_sig_i)
             s) < (eval (ENum (4)) s))%Z)),120%positive)::
             (44%positive,(AGuard (fun s => ((eval (EVar IDcheck_key_sig_i)
             s) >= (eval (ENum (4)) s))%Z)),45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AAssign IDcheck_key_sig_i (Some (ENum (0)))),
             47%positive)::(47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard (fun s => ((eval (EVar IDcheck_key_sig_i)
             s) < (eval (ENum (8)) s))%Z)),113%positive)::
             (49%positive,(AGuard (fun s => ((eval (EVar IDcheck_key_sig_i)
             s) >= (eval (ENum (8)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AAssign IDcheck_key_sig_algorithm None),
             52%positive)::(52%positive,AWeaken,53%positive)::
             (53%positive,ANone,110%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDcheck_key_sig_algorithm None),
             55%positive)::(55%positive,AWeaken,56%positive)::
             (56%positive,ANone,107%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDcheck_key_sig_mdlow2_off0 None),
             58%positive)::
             (58%positive,(AAssign IDcheck_key_sig_mdlow2_off1 None),
             59%positive)::(59%positive,AWeaken,60%positive)::
             (60%positive,ANone,104%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,101%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,ANone,98%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDcheck_key_sig_count None),66%positive)::
             (66%positive,AWeaken,67%positive)::
             (67%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_count) s) <
             (eval (ENum (0)) s))%Z)),94%positive)::
             (67%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_count) s) >=
             (eval (ENum (0)) s))%Z)),68%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_count) s) <>
             (eval (ENum (16)) s))%Z)),90%positive)::
             (69%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_count) s) =
             (eval (ENum (16)) s))%Z)),70%positive)::
             (70%positive,AWeaken,71%positive)::
             (71%positive,ANone,87%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,AWeaken,73%positive)::
             (73%positive,ANone,87%positive)::
             (73%positive,ANone,74%positive)::
             (74%positive,AWeaken,75%positive)::
             (75%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) <>
             (eval (ENum (32)) s))%Z)),77%positive)::
             (75%positive,(AGuard
             (fun s => ((eval (EVar IDcheck_key_sig_class) s) =
             (eval (ENum (32)) s))%Z)),76%positive)::
             (76%positive,AWeaken,80%positive)::
             (77%positive,AWeaken,78%positive)::
             (78%positive,ANone,79%positive)::
             (79%positive,AWeaken,80%positive)::
             (80%positive,ANone,84%positive)::
             (80%positive,ANone,81%positive)::
             (81%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (0)))),
             82%positive)::(82%positive,ANone,83%positive)::
             (83%positive,AWeaken,143%positive)::
             (84%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-20)))),
             85%positive)::(85%positive,ANone,86%positive)::
             (86%positive,AWeaken,143%positive)::
             (87%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-9)))),
             88%positive)::(88%positive,ANone,89%positive)::
             (89%positive,AWeaken,143%positive)::
             (90%positive,AWeaken,91%positive)::
             (91%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-9)))),
             92%positive)::(92%positive,ANone,93%positive)::
             (93%positive,AWeaken,143%positive)::
             (94%positive,AWeaken,95%positive)::
             (95%positive,(AAssign IDcheck_key_sig__tmp
             (Some (EVar IDcheck_key_sig_count))),96%positive)::
             (96%positive,ANone,97%positive)::
             (97%positive,AWeaken,143%positive)::
             (98%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-10)))),
             99%positive)::(99%positive,ANone,100%positive)::
             (100%positive,AWeaken,143%positive)::
             (101%positive,(AAssign IDcheck_key_sig__tmp
             (Some (ENum (-10)))),102%positive)::
             (102%positive,ANone,103%positive)::
             (103%positive,AWeaken,143%positive)::
             (104%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-2)))),
             105%positive)::(105%positive,ANone,106%positive)::
             (106%positive,AWeaken,143%positive)::
             (107%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-7)))),
             108%positive)::(108%positive,ANone,109%positive)::
             (109%positive,AWeaken,143%positive)::
             (110%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-6)))),
             111%positive)::(111%positive,ANone,112%positive)::
             (112%positive,AWeaken,143%positive)::
             (113%positive,AWeaken,114%positive)::
             (114%positive,ANone,115%positive)::
             (115%positive,(AAssign IDcheck_key_sig_i
             (Some (EAdd (EVar IDcheck_key_sig_i) (ENum (1))))),116%positive)::
             (116%positive,ANone,117%positive)::
             (117%positive,ANone,118%positive)::
             (118%positive,(AAssign IDcheck_key_sig_z (Some (EAdd (ENum (1))
             (EVar IDcheck_key_sig_z)))),119%positive)::
             (119%positive,AWeaken,49%positive)::
             (120%positive,AWeaken,121%positive)::
             (121%positive,ANone,122%positive)::
             (122%positive,(AAssign IDcheck_key_sig_i
             (Some (EAdd (EVar IDcheck_key_sig_i) (ENum (1))))),123%positive)::
             (123%positive,ANone,124%positive)::
             (124%positive,ANone,125%positive)::
             (125%positive,(AAssign IDcheck_key_sig_z (Some (EAdd (ENum (1))
             (EVar IDcheck_key_sig_z)))),126%positive)::
             (126%positive,AWeaken,44%positive)::
             (127%positive,AWeaken,128%positive)::
             (128%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-8)))),
             129%positive)::(129%positive,ANone,130%positive)::
             (130%positive,AWeaken,143%positive)::
             (131%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-8)))),
             132%positive)::(132%positive,ANone,133%positive)::
             (133%positive,AWeaken,143%positive)::
             (134%positive,(AAssign IDcheck_key_sig__tmp (Some (ENum (-8)))),
             135%positive)::(135%positive,ANone,136%positive)::
             (136%positive,AWeaken,143%positive)::
             (137%positive,ANone,140%positive)::
             (138%positive,AWeaken,139%positive)::
             (139%positive,ANone,140%positive)::
             (140%positive,(AAssign IDcheck_key_sig__tmp
             (Some (ENum (-10)))),141%positive)::
             (141%positive,ANone,142%positive)::
             (142%positive,AWeaken,143%positive)::nil
|}.

Definition check_key_sig_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_cert_length) <= 0)%Z
    | 4%positive => (-1 * (s IDcheck_key_sig_cert_length) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_cert_length) <= 0)%Z
    | 6%positive => (-1 * (s IDcheck_key_sig_cert_length) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_cert_length) <= 0)%Z
    | 8%positive => (-1 * (s IDcheck_key_sig_cert_length) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 9%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_cert_length) <= 0)%Z
    | 10%positive => (-1 * (s IDcheck_key_sig_cert_length) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 11%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_cert_length) <= 0)%Z
    | 12%positive => (-1 * (s IDcheck_key_sig_cert_length) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 13%positive => (-1 * (s IDcheck_key_sig_cert_length) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 14%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 15%positive => (1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 16%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 17%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 18%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 19%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 20%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 21%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 22%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 23%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 24%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 25%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 26%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 27%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 28%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0)%Z
    | 29%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 30%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 31%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -17 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 17 <= 0)%Z
    | 32%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 33%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 34%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -18 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 18 <= 0)%Z
    | 35%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 36%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 37%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -19 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 19 <= 0)%Z
    | 38%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 39%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 40%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 32 <= 0)%Z
    | 41%positive => (-1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 42%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0)%Z
    | 43%positive => (-1 * (s IDcheck_key_sig_i) <= 0 /\ 1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 44%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -4 <= 0)%Z
    | 45%positive => (1 * (s IDcheck_key_sig_i) + -4 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 4 <= 0)%Z
    | 46%positive => (-1 * (s IDcheck_key_sig_i) + 4 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -4 <= 0)%Z
    | 47%positive => (-1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0)%Z
    | 48%positive => (-1 * (s IDcheck_key_sig_i) <= 0 /\ 1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0)%Z
    | 49%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 50%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 51%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 52%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 53%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 54%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 55%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 56%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 57%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 58%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 59%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 60%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 61%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 62%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 63%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 64%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 65%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 66%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 67%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 68%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_count) <= 0)%Z
    | 69%positive => (-1 * (s IDcheck_key_sig_count) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 70%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0)%Z
    | 71%positive => (-1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 72%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0)%Z
    | 73%positive => (-1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 74%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0)%Z
    | 75%positive => (-1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 76%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 32 <= 0)%Z
    | 77%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -31 <= 0)%Z
    | 78%positive => (1 * (s IDcheck_key_sig_class) + -31 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 79%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -31 <= 0)%Z
    | 80%positive => (1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 81%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0)%Z
    | 82%positive => (1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) <= 0 /\ -1 * (s IDcheck_key_sig__tmp) <= 0)%Z
    | 83%positive => (-1 * (s IDcheck_key_sig__tmp) <= 0 /\ 1 * (s IDcheck_key_sig__tmp) <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0)%Z
    | 84%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0)%Z
    | 85%positive => (1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 20 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -20 <= 0)%Z
    | 86%positive => (-1 * (s IDcheck_key_sig__tmp) + -20 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 20 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0)%Z
    | 87%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0)%Z
    | 88%positive => (-1 * (s IDcheck_key_sig_count) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 9 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -9 <= 0)%Z
    | 89%positive => (-1 * (s IDcheck_key_sig__tmp) + -9 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 9 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + -16 <= 0 /\ -1 * (s IDcheck_key_sig_count) + 16 <= 0)%Z
    | 90%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_count) <= 0)%Z
    | 91%positive => (-1 * (s IDcheck_key_sig_count) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 92%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_count) <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 9 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -9 <= 0)%Z
    | 93%positive => (-1 * (s IDcheck_key_sig__tmp) + -9 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 9 <= 0 /\ -1 * (s IDcheck_key_sig_count) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 94%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + 1 <= 0)%Z
    | 95%positive => (1 * (s IDcheck_key_sig_count) + 1 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 96%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_count) + 1 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 1 <= 0)%Z
    | 97%positive => (1 * (s IDcheck_key_sig__tmp) + 1 <= 0 /\ 1 * (s IDcheck_key_sig_count) + 1 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 98%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 99%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 10 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -10 <= 0)%Z
    | 100%positive => (-1 * (s IDcheck_key_sig__tmp) + -10 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 10 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 101%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 102%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 10 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -10 <= 0)%Z
    | 103%positive => (-1 * (s IDcheck_key_sig__tmp) + -10 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 10 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 104%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 105%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 2 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -2 <= 0)%Z
    | 106%positive => (-1 * (s IDcheck_key_sig__tmp) + -2 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 2 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 107%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 108%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 7 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -7 <= 0)%Z
    | 109%positive => (-1 * (s IDcheck_key_sig__tmp) + -7 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 7 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 110%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 111%positive => (-1 * (s IDcheck_key_sig_i) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 6 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -6 <= 0)%Z
    | 112%positive => (-1 * (s IDcheck_key_sig__tmp) + -6 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 6 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) + 8 <= 0)%Z
    | 113%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_i) + -7 <= 0)%Z
    | 114%positive => (1 * (s IDcheck_key_sig_i) + -7 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 115%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_i) + -7 <= 0)%Z
    | 116%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 117%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 118%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -8 <= 0)%Z
    | 119%positive => (1 * (s IDcheck_key_sig_i) + -8 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_z) + 1 <= 0)%Z
    | 120%positive => (-1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_i) + -3 <= 0)%Z
    | 121%positive => (1 * (s IDcheck_key_sig_i) + -3 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0)%Z
    | 122%positive => (-1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_i) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_i) + -3 <= 0)%Z
    | 123%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -4 <= 0)%Z
    | 124%positive => (1 * (s IDcheck_key_sig_i) + -4 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 125%positive => (-1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ 1 * (s IDcheck_key_sig_i) + -4 <= 0)%Z
    | 126%positive => (1 * (s IDcheck_key_sig_i) + -4 <= 0 /\ -1 * (s IDcheck_key_sig_i) + 1 <= 0 /\ -1 * (s IDcheck_key_sig_class) + 16 <= 0 /\ 1 * (s IDcheck_key_sig_class) + -32 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ -1 * (s IDcheck_key_sig_z) + 1 <= 0)%Z
    | 127%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 128%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 129%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 8 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -8 <= 0)%Z
    | 130%positive => (-1 * (s IDcheck_key_sig__tmp) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 131%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 132%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 8 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -8 <= 0)%Z
    | 133%positive => (-1 * (s IDcheck_key_sig__tmp) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 8 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 134%positive => (1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 135%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 8 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -8 <= 0)%Z
    | 136%positive => (-1 * (s IDcheck_key_sig__tmp) + -8 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 8 <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 137%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_cert_length) + -283 <= 0)%Z
    | 138%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_cert_length) + 284 <= 0)%Z
    | 139%positive => (-1 * (s IDcheck_key_sig_cert_length) + 284 <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | 140%positive => (-1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 141%positive => (1 * (s IDcheck_key_sig_z) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 10 <= 0 /\ -1 * (s IDcheck_key_sig__tmp) + -10 <= 0)%Z
    | 142%positive => (-1 * (s IDcheck_key_sig__tmp) + -10 <= 0 /\ 1 * (s IDcheck_key_sig__tmp) + 10 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_z) <= 0)%Z
    | 143%positive => (1 * (s IDcheck_key_sig__tmp) <= 0 /\ -1 * (s IDcheck_key_sig_z) <= 0 /\ 1 * (s IDcheck_key_sig_global_precision) + -130 <= 0 /\ -1 * (s IDcheck_key_sig_global_precision) + 130 <= 0)%Z
    | _ => False
  end.

Definition check_key_sig_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((12 # 1))%Q
    | 2%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 3%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 4%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 5%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 6%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 7%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 8%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 9%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 10%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 11%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 12%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 13%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 14%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 15%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 16%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 17%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 18%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 19%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 20%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 21%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 22%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 23%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 24%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 25%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 26%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 27%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 28%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 29%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 30%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 31%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 32%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision))
                      + max0((s IDcheck_key_sig_z)))%Q
    | 33%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 34%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 35%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 36%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 37%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 38%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 39%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 40%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 41%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 42%positive => (-(92 # 7)
                      + (6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (8 # 7) * (s IDcheck_key_sig_i)
                      + max0(4 - (s IDcheck_key_sig_i))
                      + (8 # 7) * max0(8 - (s IDcheck_key_sig_i))
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 43%positive => (-(92 # 7)
                      + (6 # 65) * (s IDcheck_key_sig_global_precision)
                      + (8 # 7) * (s IDcheck_key_sig_i)
                      + max0(4 - (s IDcheck_key_sig_i))
                      + (8 # 7) * max0(8 - (s IDcheck_key_sig_i))
                      + (6 # 65) * max0(130
                                        - (s IDcheck_key_sig_global_precision)))%Q
    | 44%positive => ((8 # 1) + (s IDcheck_key_sig_z)
                      + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 45%positive => ((8 # 1) + (s IDcheck_key_sig_z)
                      + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 46%positive => ((8 # 1) + (s IDcheck_key_sig_z))%Q
    | 47%positive => ((8 # 1) - (s IDcheck_key_sig_i) + (s IDcheck_key_sig_z))%Q
    | 48%positive => ((8 # 1) - (s IDcheck_key_sig_i) + (s IDcheck_key_sig_z))%Q
    | 49%positive => ((8 # 1) - (s IDcheck_key_sig_i) + (s IDcheck_key_sig_z))%Q
    | 50%positive => ((8 # 1) - (s IDcheck_key_sig_i) + (s IDcheck_key_sig_z))%Q
    | 51%positive => ((8 # 1) - (s IDcheck_key_sig_i) + (s IDcheck_key_sig_z))%Q
    | 52%positive => ((8 # 1) - (s IDcheck_key_sig_i) + (s IDcheck_key_sig_z))%Q
    | 53%positive => ((s IDcheck_key_sig_z) + max0(8 - (s IDcheck_key_sig_i)))%Q
    | 54%positive => ((s IDcheck_key_sig_z) + max0(8 - (s IDcheck_key_sig_i)))%Q
    | 55%positive => ((s IDcheck_key_sig_z) + max0(8 - (s IDcheck_key_sig_i)))%Q
    | 56%positive => ((s IDcheck_key_sig_z) + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 57%positive => ((s IDcheck_key_sig_z) + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 58%positive => ((s IDcheck_key_sig_z) + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 59%positive => ((s IDcheck_key_sig_z) + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 60%positive => ((s IDcheck_key_sig_z) + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 61%positive => ((s IDcheck_key_sig_z) + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 62%positive => ((s IDcheck_key_sig_z))%Q
    | 63%positive => ((s IDcheck_key_sig_z))%Q
    | 64%positive => ((s IDcheck_key_sig_z))%Q
    | 65%positive => ((s IDcheck_key_sig_z))%Q
    | 66%positive => ((s IDcheck_key_sig_z))%Q
    | 67%positive => ((s IDcheck_key_sig_z))%Q
    | 68%positive => ((s IDcheck_key_sig_z))%Q
    | 69%positive => ((s IDcheck_key_sig_z))%Q
    | 70%positive => ((s IDcheck_key_sig_z))%Q
    | 71%positive => ((s IDcheck_key_sig_z))%Q
    | 72%positive => ((s IDcheck_key_sig_z))%Q
    | 73%positive => ((s IDcheck_key_sig_z))%Q
    | 74%positive => ((s IDcheck_key_sig_z))%Q
    | 75%positive => ((s IDcheck_key_sig_z))%Q
    | 76%positive => ((s IDcheck_key_sig_z))%Q
    | 77%positive => ((s IDcheck_key_sig_z))%Q
    | 78%positive => ((s IDcheck_key_sig_z))%Q
    | 79%positive => ((s IDcheck_key_sig_z))%Q
    | 80%positive => ((s IDcheck_key_sig_z))%Q
    | 81%positive => ((s IDcheck_key_sig_z))%Q
    | 82%positive => ((s IDcheck_key_sig_z))%Q
    | 83%positive => ((s IDcheck_key_sig_z))%Q
    | 84%positive => ((s IDcheck_key_sig_z))%Q
    | 85%positive => ((s IDcheck_key_sig_z))%Q
    | 86%positive => ((s IDcheck_key_sig_z))%Q
    | 87%positive => ((s IDcheck_key_sig_z))%Q
    | 88%positive => ((s IDcheck_key_sig_z))%Q
    | 89%positive => ((s IDcheck_key_sig_z))%Q
    | 90%positive => ((s IDcheck_key_sig_z))%Q
    | 91%positive => ((s IDcheck_key_sig_z))%Q
    | 92%positive => ((s IDcheck_key_sig_z))%Q
    | 93%positive => ((s IDcheck_key_sig_z))%Q
    | 94%positive => ((s IDcheck_key_sig_z))%Q
    | 95%positive => ((s IDcheck_key_sig_z))%Q
    | 96%positive => ((s IDcheck_key_sig_z))%Q
    | 97%positive => ((s IDcheck_key_sig_z))%Q
    | 98%positive => ((s IDcheck_key_sig_z))%Q
    | 99%positive => ((s IDcheck_key_sig_z))%Q
    | 100%positive => ((s IDcheck_key_sig_z))%Q
    | 101%positive => ((s IDcheck_key_sig_z))%Q
    | 102%positive => ((s IDcheck_key_sig_z))%Q
    | 103%positive => ((s IDcheck_key_sig_z))%Q
    | 104%positive => ((s IDcheck_key_sig_z)
                       + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 105%positive => ((s IDcheck_key_sig_z)
                       + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 106%positive => ((s IDcheck_key_sig_z)
                       + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 107%positive => ((s IDcheck_key_sig_z)
                       + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 108%positive => ((s IDcheck_key_sig_z)
                       + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 109%positive => ((s IDcheck_key_sig_z)
                       + max0(7 - (s IDcheck_key_sig_i)))%Q
    | 110%positive => ((s IDcheck_key_sig_z)
                       + max0(8 - (s IDcheck_key_sig_i)))%Q
    | 111%positive => ((s IDcheck_key_sig_z)
                       + max0(8 - (s IDcheck_key_sig_i)))%Q
    | 112%positive => ((s IDcheck_key_sig_z)
                       + max0(8 - (s IDcheck_key_sig_i)))%Q
    | 113%positive => ((8 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 114%positive => ((8 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 115%positive => ((8 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 116%positive => ((9 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 117%positive => ((9 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 118%positive => ((9 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 119%positive => ((8 # 1) - (s IDcheck_key_sig_i)
                       + (s IDcheck_key_sig_z))%Q
    | 120%positive => ((8 # 1) + (s IDcheck_key_sig_z)
                       + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 121%positive => ((9 # 1) + (s IDcheck_key_sig_z)
                       + max0(3 - (s IDcheck_key_sig_i)))%Q
    | 122%positive => ((9 # 1) + (s IDcheck_key_sig_z)
                       + max0(3 - (s IDcheck_key_sig_i)))%Q
    | 123%positive => ((9 # 1) + (s IDcheck_key_sig_z)
                       + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 124%positive => ((9 # 1) + (s IDcheck_key_sig_z)
                       + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 125%positive => ((9 # 1) + (s IDcheck_key_sig_z)
                       + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 126%positive => ((8 # 1) + (s IDcheck_key_sig_z)
                       + max0(4 - (s IDcheck_key_sig_i)))%Q
    | 127%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision)))%Q
    | 128%positive => (0)%Q
    | 129%positive => (0)%Q
    | 130%positive => (0)%Q
    | 131%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision))
                       + max0((s IDcheck_key_sig_z)))%Q
    | 132%positive => (-(12 # 1)
                       + (6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (12 # 7) * max0(-1 - (s IDcheck_key_sig__tmp))
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision))
                       + max0((s IDcheck_key_sig_z)))%Q
    | 133%positive => (-(12 # 1)
                       + (6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (12 # 7) * max0(-1 - (s IDcheck_key_sig__tmp))
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision))
                       + max0((s IDcheck_key_sig_z)))%Q
    | 134%positive => ((6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision))
                       + max0((s IDcheck_key_sig_z)))%Q
    | 135%positive => (-(12 # 1)
                       + (6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (12 # 7) * max0(-1 - (s IDcheck_key_sig__tmp))
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision))
                       + max0((s IDcheck_key_sig_z)))%Q
    | 136%positive => (-(12 # 1)
                       + (6 # 65) * (s IDcheck_key_sig_global_precision)
                       + (12 # 7) * max0(-1 - (s IDcheck_key_sig__tmp))
                       + (6 # 65) * max0(130
                                         - (s IDcheck_key_sig_global_precision))
                       + max0((s IDcheck_key_sig_z)))%Q
    | 137%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 138%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 139%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 140%positive => ((12 # 1) + (s IDcheck_key_sig_z))%Q
    | 141%positive => ((s IDcheck_key_sig_z)
                       + (3 # 1) * max0(-6 - (s IDcheck_key_sig__tmp)))%Q
    | 142%positive => ((s IDcheck_key_sig_z)
                       + (3 # 1) * max0(-6 - (s IDcheck_key_sig__tmp)))%Q
    | 143%positive => ((s IDcheck_key_sig_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition check_key_sig_hints (p : node) (s : state) := 
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
    | 19%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcheck_key_sig_z)) (0))) (F_max0_ge_0 ((s IDcheck_key_sig_z)));
                      (*-0.0923077 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (130
                                                                    - (s IDcheck_key_sig_global_precision)) (0))) (F_max0_ge_0 (130
                                                                    - (s IDcheck_key_sig_global_precision)))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcheck_key_sig_z))) (F_check_ge (0) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcheck_key_sig_z))) (F_check_ge (0) (0))]
    | 32%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcheck_key_sig_z))) (F_check_ge (0) (0))]
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
    | 43%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcheck_key_sig_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcheck_key_sig_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcheck_key_sig_z)));
                      (*0 0.0923077*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                                    - (s IDcheck_key_sig_global_precision))) (F_check_ge (130
                                                                    - (s IDcheck_key_sig_global_precision)) (0));
                      (*-1.14286 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDcheck_key_sig_i))) (F_check_ge (8
                                                                    - (s IDcheck_key_sig_i)) (0))]
    | 44%positive => []
    | 45%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDcheck_key_sig_i)) (3
                                                                    - (s IDcheck_key_sig_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDcheck_key_sig_i))]
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDcheck_key_sig_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDcheck_key_sig_i)))]
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*0 1*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDcheck_key_sig_i)) (7
                                                                    - (s IDcheck_key_sig_i)))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                - (s IDcheck_key_sig_i))) (F_check_ge (0) (0))]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => []
    | 75%positive => []
    | 76%positive => []
    | 77%positive => []
    | 78%positive => []
    | 79%positive => []
    | 80%positive => []
    | 81%positive => []
    | 82%positive => []
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => []
    | 94%positive => []
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => []
    | 103%positive => []
    | 104%positive => []
    | 105%positive => []
    | 106%positive => [(*-1 0*) F_max0_ge_0 (7 - (s IDcheck_key_sig_i))]
    | 107%positive => []
    | 108%positive => []
    | 109%positive => [(*-1 0*) F_max0_ge_0 (7 - (s IDcheck_key_sig_i))]
    | 110%positive => []
    | 111%positive => []
    | 112%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                              - (s IDcheck_key_sig_i)) (7
                                                                    - (s IDcheck_key_sig_i)));
                       (*-1 0*) F_max0_ge_0 (7 - (s IDcheck_key_sig_i))]
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => []
    | 117%positive => []
    | 118%positive => []
    | 119%positive => []
    | 120%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                      - (s IDcheck_key_sig_i)) (1)]
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => [(*0 12*) F_one;
                       (*0 0.0923077*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                                    - (s IDcheck_key_sig_global_precision))) (F_check_ge (130
                                                                    - (s IDcheck_key_sig_global_precision)) (0))]
    | 128%positive => []
    | 129%positive => []
    | 130%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcheck_key_sig_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcheck_key_sig_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcheck_key_sig_z)))]
    | 131%positive => []
    | 132%positive => []
    | 133%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcheck_key_sig_z))) (F_check_ge ((s IDcheck_key_sig_z)) (0));
                       (*-0.0923077 0*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                                    - (s IDcheck_key_sig_global_precision))) (F_check_ge (130
                                                                    - (s IDcheck_key_sig_global_precision)) (0));
                       (*-1.71429 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDcheck_key_sig__tmp))) (F_check_ge (0) (0))]
    | 134%positive => []
    | 135%positive => []
    | 136%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcheck_key_sig_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcheck_key_sig_z))) (F_check_ge (0) (0));
                       (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcheck_key_sig_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcheck_key_sig_z)));
                       (*-0.0923077 0*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                                    - (s IDcheck_key_sig_global_precision))) (F_check_ge (130
                                                                    - (s IDcheck_key_sig_global_precision)) (0));
                       (*-1.71429 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDcheck_key_sig__tmp))) (F_check_ge (0) (0))]
    | 137%positive => []
    | 138%positive => []
    | 139%positive => []
    | 140%positive => []
    | 141%positive => []
    | 142%positive => [(*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                  - (s IDcheck_key_sig__tmp))) (F_check_ge (0) (0))]
    | 143%positive => []
    | _ => []
  end.


Theorem check_key_sig_ai_correct:
  forall s p' s', steps (g_start check_key_sig) s (g_edges check_key_sig) p' s' -> check_key_sig_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem check_key_sig_pot_correct:
  forall s p' s',
    steps (g_start check_key_sig) s (g_edges check_key_sig) p' s' ->
    (check_key_sig_pot (g_start check_key_sig) s >= check_key_sig_pot p' s')%Q.
Proof.
  check_lp check_key_sig_ai_correct check_key_sig_hints.
Qed.

