Require Import pasta.Pasta.

Notation IDBF_cbc_encrypt_z := 1%positive.
Notation IDBF_cbc_encrypt__tmp := 2%positive.
Notation IDBF_cbc_encrypt__tmp1 := 3%positive.
Notation IDBF_cbc_encrypt_l := 4%positive.
Notation IDBF_cbc_encrypt_tin0 := 5%positive.
Notation IDBF_cbc_encrypt_tin1 := 6%positive.
Notation IDBF_cbc_encrypt_tout0 := 7%positive.
Notation IDBF_cbc_encrypt_tout1 := 8%positive.
Notation IDBF_cbc_encrypt_xor0 := 9%positive.
Notation IDBF_cbc_encrypt_xor1 := 10%positive.
Notation IDBF_cbc_encrypt_encrypt := 11%positive.
Notation IDBF_cbc_encrypt_in := 12%positive.
Notation IDBF_cbc_encrypt_iv := 13%positive.
Notation IDBF_cbc_encrypt_ks := 14%positive.
Notation IDBF_cbc_encrypt_length := 15%positive.
Notation IDBF_cbc_encrypt_out := 16%positive.
Definition BF_cbc_encrypt : graph := {|
  g_start := 1%positive;
  g_end := 117%positive;
  g_edges := (1%positive,(AAssign IDBF_cbc_encrypt_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDBF_cbc_encrypt__tmp1
             (Some (EVar IDBF_cbc_encrypt_length))),3%positive)::
             (3%positive,(AAssign IDBF_cbc_encrypt__tmp
             (Some (EVar IDBF_cbc_encrypt_encrypt))),4%positive)::
             (4%positive,(AAssign IDBF_cbc_encrypt_l
             (Some (EVar IDBF_cbc_encrypt__tmp1))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDBF_cbc_encrypt__tmp) s) <>
             (eval (ENum (0)) s))%Z)),67%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDBF_cbc_encrypt__tmp) s) =
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDBF_cbc_encrypt_xor0 None),9%positive)::
             (9%positive,(AAssign IDBF_cbc_encrypt_xor0 None),10%positive)::
             (10%positive,(AAssign IDBF_cbc_encrypt_xor0 None),11%positive)::
             (11%positive,(AAssign IDBF_cbc_encrypt_xor0 None),12%positive)::
             (12%positive,(AAssign IDBF_cbc_encrypt_xor1 None),13%positive)::
             (13%positive,(AAssign IDBF_cbc_encrypt_xor1 None),14%positive)::
             (14%positive,(AAssign IDBF_cbc_encrypt_xor1 None),15%positive)::
             (15%positive,(AAssign IDBF_cbc_encrypt_xor1 None),16%positive)::
             (16%positive,(AAssign IDBF_cbc_encrypt_l
             (Some (ESub (EVar IDBF_cbc_encrypt_l) (ENum (8))))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) >= (eval (ENum (0)) s))%Z)),48%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) < (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) <> (eval (ENum (-8)) s))%Z)),23%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) = (eval (ENum (-8)) s))%Z)),22%positive)::
             (22%positive,AWeaken,47%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDBF_cbc_encrypt_tin0 None),25%positive)::
             (25%positive,(AAssign IDBF_cbc_encrypt_tin0 None),26%positive)::
             (26%positive,(AAssign IDBF_cbc_encrypt_tin0 None),27%positive)::
             (27%positive,(AAssign IDBF_cbc_encrypt_tin0 None),28%positive)::
             (28%positive,(AAssign IDBF_cbc_encrypt_tin1 None),29%positive)::
             (29%positive,(AAssign IDBF_cbc_encrypt_tin1 None),30%positive)::
             (30%positive,(AAssign IDBF_cbc_encrypt_tin1 None),31%positive)::
             (31%positive,(AAssign IDBF_cbc_encrypt_tin1 None),32%positive)::
             (32%positive,(AAssign IDBF_cbc_encrypt_tout0 None),33%positive)::
             (33%positive,(AAssign IDBF_cbc_encrypt_tout1 None),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,44%positive)::
             (35%positive,ANone,36%positive)::
             (35%positive,ANone,37%positive)::
             (35%positive,ANone,38%positive)::
             (35%positive,ANone,39%positive)::
             (35%positive,ANone,40%positive)::
             (35%positive,ANone,41%positive)::
             (35%positive,ANone,42%positive)::
             (35%positive,ANone,43%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDBF_cbc_encrypt_xor0
             (Some (EVar IDBF_cbc_encrypt_tin0))),45%positive)::
             (45%positive,(AAssign IDBF_cbc_encrypt_xor1
             (Some (EVar IDBF_cbc_encrypt_tin1))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,110%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDBF_cbc_encrypt_tin0 None),50%positive)::
             (50%positive,(AAssign IDBF_cbc_encrypt_tin0 None),51%positive)::
             (51%positive,(AAssign IDBF_cbc_encrypt_tin0 None),52%positive)::
             (52%positive,(AAssign IDBF_cbc_encrypt_tin0 None),53%positive)::
             (53%positive,(AAssign IDBF_cbc_encrypt_tin1 None),54%positive)::
             (54%positive,(AAssign IDBF_cbc_encrypt_tin1 None),55%positive)::
             (55%positive,(AAssign IDBF_cbc_encrypt_tin1 None),56%positive)::
             (56%positive,(AAssign IDBF_cbc_encrypt_tin1 None),57%positive)::
             (57%positive,(AAssign IDBF_cbc_encrypt_tout0 None),58%positive)::
             (58%positive,(AAssign IDBF_cbc_encrypt_tout1 None),59%positive)::
             (59%positive,(AAssign IDBF_cbc_encrypt_xor0
             (Some (EVar IDBF_cbc_encrypt_tin0))),60%positive)::
             (60%positive,(AAssign IDBF_cbc_encrypt_xor1
             (Some (EVar IDBF_cbc_encrypt_tin1))),61%positive)::
             (61%positive,ANone,62%positive)::
             (62%positive,(AAssign IDBF_cbc_encrypt_l
             (Some (ESub (EVar IDBF_cbc_encrypt_l) (ENum (8))))),63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDBF_cbc_encrypt_z (Some (EAdd (ENum (1))
             (EVar IDBF_cbc_encrypt_z)))),66%positive)::
             (66%positive,AWeaken,19%positive)::
             (67%positive,AWeaken,68%positive)::
             (68%positive,(AAssign IDBF_cbc_encrypt_tout0 None),69%positive)::
             (69%positive,(AAssign IDBF_cbc_encrypt_tout0 None),70%positive)::
             (70%positive,(AAssign IDBF_cbc_encrypt_tout0 None),71%positive)::
             (71%positive,(AAssign IDBF_cbc_encrypt_tout0 None),72%positive)::
             (72%positive,(AAssign IDBF_cbc_encrypt_tout1 None),73%positive)::
             (73%positive,(AAssign IDBF_cbc_encrypt_tout1 None),74%positive)::
             (74%positive,(AAssign IDBF_cbc_encrypt_tout1 None),75%positive)::
             (75%positive,(AAssign IDBF_cbc_encrypt_tout1 None),76%positive)::
             (76%positive,(AAssign IDBF_cbc_encrypt_l
             (Some (ESub (EVar IDBF_cbc_encrypt_l) (ENum (8))))),77%positive)::
             (77%positive,ANone,78%positive)::
             (78%positive,AWeaken,79%positive)::
             (79%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) >= (eval (ENum (0)) s))%Z)),118%positive)::
             (79%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) < (eval (ENum (0)) s))%Z)),80%positive)::
             (80%positive,AWeaken,81%positive)::
             (81%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) <> (eval (ENum (-8)) s))%Z)),83%positive)::
             (81%positive,(AGuard (fun s => ((eval (EVar IDBF_cbc_encrypt_l)
             s) = (eval (ENum (-8)) s))%Z)),82%positive)::
             (82%positive,AWeaken,109%positive)::
             (83%positive,AWeaken,84%positive)::
             (84%positive,(AAssign IDBF_cbc_encrypt_tin1 (Some (ENum (0)))),
             85%positive)::
             (85%positive,(AAssign IDBF_cbc_encrypt_tin0 (Some (ENum (0)))),
             86%positive)::(86%positive,AWeaken,87%positive)::
             (87%positive,ANone,104%positive)::
             (87%positive,ANone,88%positive)::
             (87%positive,ANone,90%positive)::
             (87%positive,ANone,92%positive)::
             (87%positive,ANone,94%positive)::
             (87%positive,ANone,96%positive)::
             (87%positive,ANone,98%positive)::
             (87%positive,ANone,100%positive)::
             (87%positive,ANone,102%positive)::
             (88%positive,(AAssign IDBF_cbc_encrypt_tin1 None),89%positive)::
             (89%positive,ANone,90%positive)::
             (90%positive,(AAssign IDBF_cbc_encrypt_tin1 None),91%positive)::
             (91%positive,ANone,92%positive)::
             (92%positive,(AAssign IDBF_cbc_encrypt_tin1 None),93%positive)::
             (93%positive,ANone,94%positive)::
             (94%positive,(AAssign IDBF_cbc_encrypt_tin1 None),95%positive)::
             (95%positive,ANone,96%positive)::
             (96%positive,(AAssign IDBF_cbc_encrypt_tin0 None),97%positive)::
             (97%positive,ANone,98%positive)::
             (98%positive,(AAssign IDBF_cbc_encrypt_tin0 None),99%positive)::
             (99%positive,ANone,100%positive)::
             (100%positive,(AAssign IDBF_cbc_encrypt_tin0 None),101%positive)::
             (101%positive,ANone,102%positive)::
             (102%positive,(AAssign IDBF_cbc_encrypt_tin0 None),103%positive)::
             (103%positive,ANone,104%positive)::
             (104%positive,(AAssign IDBF_cbc_encrypt_tin0 None),105%positive)::
             (105%positive,(AAssign IDBF_cbc_encrypt_tin1 None),106%positive)::
             (106%positive,(AAssign IDBF_cbc_encrypt_tout0 None),
             107%positive)::
             (107%positive,(AAssign IDBF_cbc_encrypt_tout1 None),
             108%positive)::(108%positive,ANone,109%positive)::
             (109%positive,ANone,110%positive)::
             (110%positive,(AAssign IDBF_cbc_encrypt_xor1 (Some (ENum (0)))),
             111%positive)::
             (111%positive,(AAssign IDBF_cbc_encrypt_xor0 (Some (ENum (0)))),
             112%positive)::
             (112%positive,(AAssign IDBF_cbc_encrypt_tout1
             (Some (ENum (0)))),113%positive)::
             (113%positive,(AAssign IDBF_cbc_encrypt_tout0
             (Some (ENum (0)))),114%positive)::
             (114%positive,(AAssign IDBF_cbc_encrypt_tin1 (Some (ENum (0)))),
             115%positive)::
             (115%positive,(AAssign IDBF_cbc_encrypt_tin0 (Some (ENum (0)))),
             116%positive)::(116%positive,AWeaken,117%positive)::
             (118%positive,AWeaken,119%positive)::
             (119%positive,(AAssign IDBF_cbc_encrypt_tin0 None),120%positive)::
             (120%positive,(AAssign IDBF_cbc_encrypt_tin0 None),121%positive)::
             (121%positive,(AAssign IDBF_cbc_encrypt_tin0 None),122%positive)::
             (122%positive,(AAssign IDBF_cbc_encrypt_tin0 None),123%positive)::
             (123%positive,(AAssign IDBF_cbc_encrypt_tin1 None),124%positive)::
             (124%positive,(AAssign IDBF_cbc_encrypt_tin1 None),125%positive)::
             (125%positive,(AAssign IDBF_cbc_encrypt_tin1 None),126%positive)::
             (126%positive,(AAssign IDBF_cbc_encrypt_tin1 None),127%positive)::
             (127%positive,(AAssign IDBF_cbc_encrypt_tin0 None),128%positive)::
             (128%positive,(AAssign IDBF_cbc_encrypt_tin1 None),129%positive)::
             (129%positive,(AAssign IDBF_cbc_encrypt_tout0 None),
             130%positive)::
             (130%positive,(AAssign IDBF_cbc_encrypt_tout1 None),
             131%positive)::(131%positive,ANone,132%positive)::
             (132%positive,(AAssign IDBF_cbc_encrypt_l
             (Some (ESub (EVar IDBF_cbc_encrypt_l) (ENum (8))))),
             133%positive)::(133%positive,ANone,134%positive)::
             (134%positive,ANone,135%positive)::
             (135%positive,(AAssign IDBF_cbc_encrypt_z (Some (EAdd (ENum (1))
             (EVar IDBF_cbc_encrypt_z)))),136%positive)::
             (136%positive,AWeaken,79%positive)::nil
|}.

Definition BF_cbc_encrypt_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 4%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 5%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 6%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 7%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 9%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 10%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 11%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 12%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 13%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 14%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 15%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 17%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 19%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 22%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 8 <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) + -8 <= 0)%Z
    | 23%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 25%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 27%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 29%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 31%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 33%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 35%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 37%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 39%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 40%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 41%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 43%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 44%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 45%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 46%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 47%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 49%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 50%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 51%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 52%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 53%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 54%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 55%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 56%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 57%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 58%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 59%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 60%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 61%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0)%Z
    | 62%positive => (-1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 63%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) + -8 <= 0)%Z
    | 64%positive => (-1 * (s IDBF_cbc_encrypt_l) + -8 <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 65%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) + -8 <= 0)%Z
    | 66%positive => (-1 * (s IDBF_cbc_encrypt_l) + -8 <= 0 /\ -1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ 1 * (s IDBF_cbc_encrypt__tmp) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) + 1 <= 0)%Z
    | 67%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 68%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 69%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 70%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 71%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 72%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 73%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 74%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 75%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 76%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 77%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 78%positive => (1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 79%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 80%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 81%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 82%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 8 <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) + -8 <= 0)%Z
    | 83%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 84%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 85%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin1) <= 0)%Z
    | 86%positive => (-1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 87%positive => (-1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin1) <= 0)%Z
    | 88%positive => (-1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 89%positive => (-1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 90%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 91%positive => (-1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 92%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 93%positive => (-1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 94%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 95%positive => (-1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 96%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 97%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 98%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 99%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 100%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 101%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 102%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 103%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 104%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 105%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 106%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 107%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 108%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 109%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0)%Z
    | 110%positive => (1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 111%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor1) <= 0)%Z
    | 112%positive => (-1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor0) <= 0)%Z
    | 113%positive => (-1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout1) <= 0)%Z
    | 114%positive => (-1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout0) <= 0)%Z
    | 115%positive => (-1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin1) <= 0)%Z
    | 116%positive => (-1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin0) <= 0)%Z
    | 117%positive => (-1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor0) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0 /\ 1 * (s IDBF_cbc_encrypt_l) + 1 <= 0 /\ 1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_xor1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tout1) <= 0 /\ 1 * (s IDBF_cbc_encrypt_tin1) <= 0 /\ -1 * (s IDBF_cbc_encrypt_tin1) <= 0)%Z
    | 118%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 119%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 120%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 121%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 122%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 123%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 124%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 125%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 126%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 127%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 128%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 129%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 130%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 131%positive => (-1 * (s IDBF_cbc_encrypt_l) <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 132%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) <= 0)%Z
    | 133%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) + -8 <= 0)%Z
    | 134%positive => (-1 * (s IDBF_cbc_encrypt_l) + -8 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) <= 0)%Z
    | 135%positive => (-1 * (s IDBF_cbc_encrypt_z) <= 0 /\ -1 * (s IDBF_cbc_encrypt_l) + -8 <= 0)%Z
    | 136%positive => (-1 * (s IDBF_cbc_encrypt_l) + -8 <= 0 /\ -1 * (s IDBF_cbc_encrypt_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition BF_cbc_encrypt_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_length)))%Q
    | 2%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_length)))%Q
    | 3%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt__tmp1)))%Q
    | 4%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt__tmp1)))%Q
    | 5%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 6%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 7%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 8%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 9%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 10%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 11%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 12%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 13%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 14%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 15%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 16%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 17%positive => ((1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 18%positive => ((1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 19%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 20%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 21%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 22%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 23%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 24%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 25%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 26%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 27%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 28%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 29%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 30%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 31%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 32%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 33%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 34%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 35%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 36%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 37%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 38%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 39%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 40%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 41%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 42%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 43%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 44%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 45%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 46%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 47%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 48%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 49%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 50%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 51%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 52%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 53%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 54%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 55%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 56%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 57%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 58%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 59%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 60%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 61%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 62%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 63%positive => ((2 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 64%positive => ((2 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 65%positive => ((2 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 66%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                      + (s IDBF_cbc_encrypt_z))%Q
    | 67%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 68%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 69%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 70%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 71%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 72%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 73%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 74%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 75%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 76%positive => ((1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 77%positive => ((1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 78%positive => ((1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 79%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 80%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 81%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 82%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 83%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 84%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 85%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 86%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0((s IDBF_cbc_encrypt_l)))%Q
    | 87%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 88%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 89%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 90%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 91%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 92%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 93%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 94%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 95%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 96%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 97%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 98%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 99%positive => ((s IDBF_cbc_encrypt_z)
                      + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 100%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 101%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 102%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 103%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 104%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 105%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 106%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 107%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 108%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 109%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 110%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 111%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 112%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 113%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 114%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 115%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 116%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(-8 + (s IDBF_cbc_encrypt_l)))%Q
    | 117%positive => ((s IDBF_cbc_encrypt_z))%Q
    | 118%positive => ((s IDBF_cbc_encrypt_z)
                       + (1 # 8) * max0(8 + (s IDBF_cbc_encrypt_l)))%Q
    | 119%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 120%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 121%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 122%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 123%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 124%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 125%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 126%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 127%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 128%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 129%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 130%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 131%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 132%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 133%positive => ((2 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 134%positive => ((2 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 135%positive => ((2 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | 136%positive => ((1 # 1) + (1 # 8) * (s IDBF_cbc_encrypt_l)
                       + (s IDBF_cbc_encrypt_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition BF_cbc_encrypt_hints (p : node) (s : state) := 
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
    | 18%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBF_cbc_encrypt_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDBF_cbc_encrypt_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDBF_cbc_encrypt_z)))]
    | 19%positive => []
    | 20%positive => [(*0 0.125*) F_max0_monotonic (F_check_ge (8
                                                                + (s IDBF_cbc_encrypt_l)) ((s IDBF_cbc_encrypt_l)))]
    | 21%positive => []
    | 22%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDBF_cbc_encrypt_l)) (-8
                                                                    + (s IDBF_cbc_encrypt_l)))]
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
    | 33%positive => []
    | 34%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDBF_cbc_encrypt_l)) (-8
                                                                    + (s IDBF_cbc_encrypt_l)))]
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
    | 47%positive => []
    | 48%positive => [(*0 0.125*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    + (s IDBF_cbc_encrypt_l))) (F_check_ge (8
                                                                    + (s IDBF_cbc_encrypt_l)) (0))]
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
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    + (s IDBF_cbc_encrypt_l)) (0))) (F_max0_ge_0 (8
                                                                    + (s IDBF_cbc_encrypt_l)))]
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
    | 78%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDBF_cbc_encrypt_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDBF_cbc_encrypt_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDBF_cbc_encrypt_z)))]
    | 79%positive => []
    | 80%positive => [(*0 0.125*) F_max0_monotonic (F_check_ge (8
                                                                + (s IDBF_cbc_encrypt_l)) ((s IDBF_cbc_encrypt_l)))]
    | 81%positive => []
    | 82%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDBF_cbc_encrypt_l)) (-8
                                                                    + (s IDBF_cbc_encrypt_l)))]
    | 83%positive => []
    | 84%positive => []
    | 85%positive => []
    | 86%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDBF_cbc_encrypt_l)) (-8
                                                                    + (s IDBF_cbc_encrypt_l)))]
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
    | 106%positive => []
    | 107%positive => []
    | 108%positive => []
    | 109%positive => []
    | 110%positive => []
    | 111%positive => []
    | 112%positive => []
    | 113%positive => []
    | 114%positive => []
    | 115%positive => []
    | 116%positive => [(*-0.125 0*) F_max0_ge_0 (-8 + (s IDBF_cbc_encrypt_l))]
    | 117%positive => []
    | 118%positive => [(*0 0.125*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    + (s IDBF_cbc_encrypt_l))) (F_check_ge (8
                                                                    + (s IDBF_cbc_encrypt_l)) (0))]
    | 119%positive => []
    | 120%positive => []
    | 121%positive => []
    | 122%positive => []
    | 123%positive => []
    | 124%positive => []
    | 125%positive => []
    | 126%positive => []
    | 127%positive => []
    | 128%positive => []
    | 129%positive => []
    | 130%positive => []
    | 131%positive => []
    | 132%positive => []
    | 133%positive => []
    | 134%positive => []
    | 135%positive => []
    | 136%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    + (s IDBF_cbc_encrypt_l)) (0))) (F_max0_ge_0 (8
                                                                    + (s IDBF_cbc_encrypt_l)))]
    | _ => []
  end.


Theorem BF_cbc_encrypt_ai_correct:
  forall s p' s', steps (g_start BF_cbc_encrypt) s (g_edges BF_cbc_encrypt) p' s' -> BF_cbc_encrypt_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BF_cbc_encrypt_pot_correct:
  forall s p' s',
    steps (g_start BF_cbc_encrypt) s (g_edges BF_cbc_encrypt) p' s' ->
    (BF_cbc_encrypt_pot (g_start BF_cbc_encrypt) s >= BF_cbc_encrypt_pot p' s')%Q.
Proof.
  check_lp BF_cbc_encrypt_ai_correct BF_cbc_encrypt_hints.
Qed.

