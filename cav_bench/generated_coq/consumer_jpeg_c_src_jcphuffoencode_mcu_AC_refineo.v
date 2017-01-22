Require Import pasta.Pasta.

Notation IDencode_mcu_AC_refine_z := 1%positive.
Notation IDencode_mcu_AC_refine_Al := 2%positive.
Notation IDencode_mcu_AC_refine_BR := 3%positive.
Notation IDencode_mcu_AC_refine_EOB := 4%positive.
Notation IDencode_mcu_AC_refine_Se := 5%positive.
Notation IDencode_mcu_AC_refine_cinfo_dref_off272 := 6%positive.
Notation IDencode_mcu_AC_refine_cinfo_dref_off404 := 7%positive.
Notation IDencode_mcu_AC_refine_cinfo_dref_off408 := 8%positive.
Notation IDencode_mcu_AC_refine_cinfo_dref_off416 := 9%positive.
Notation IDencode_mcu_AC_refine_k := 10%positive.
Notation IDencode_mcu_AC_refine_r := 11%positive.
Notation IDencode_mcu_AC_refine_temp := 12%positive.
Notation IDencode_mcu_AC_refine_MCU_data := 13%positive.
Notation IDencode_mcu_AC_refine_cinfo := 14%positive.
Definition encode_mcu_AC_refine : graph := {|
  g_start := 1%positive;
  g_end := 45%positive;
  g_edges := (1%positive,(AAssign IDencode_mcu_AC_refine_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_BR) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDencode_mcu_AC_refine_Se
             (Some (EVar IDencode_mcu_AC_refine_cinfo_dref_off408))),
             5%positive)::
             (5%positive,(AAssign IDencode_mcu_AC_refine_Al
             (Some (EVar IDencode_mcu_AC_refine_cinfo_dref_off416))),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_cinfo_dref_off272)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_cinfo_dref_off272)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,13%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,12%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDencode_mcu_AC_refine_EOB
             (Some (ENum (0)))),14%positive)::
             (14%positive,(AAssign IDencode_mcu_AC_refine_k
             (Some (EVar IDencode_mcu_AC_refine_cinfo_dref_off404))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_k) s) <=
             (eval (EVar IDencode_mcu_AC_refine_Se) s))%Z)),82%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_k) s) >
             (eval (EVar IDencode_mcu_AC_refine_Se) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDencode_mcu_AC_refine_r
             (Some (ENum (0)))),20%positive)::
             (20%positive,(AAssign IDencode_mcu_AC_refine_BR
             (Some (ENum (0)))),21%positive)::
             (21%positive,(AAssign IDencode_mcu_AC_refine_k
             (Some (EVar IDencode_mcu_AC_refine_cinfo_dref_off404))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_k) s) <=
             (eval (EVar IDencode_mcu_AC_refine_Se) s))%Z)),46%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_k) s) >
             (eval (EVar IDencode_mcu_AC_refine_Se) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_r) s) >
             (eval (ENum (0)) s))%Z)),31%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_r) s) <=
             (eval (ENum (0)) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_BR) s) >
             (eval (ENum (0)) s))%Z)),30%positive)::
             (28%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_BR) s) <=
             (eval (ENum (0)) s))%Z)),29%positive)::
             (29%positive,AWeaken,38%positive)::
             (30%positive,AWeaken,32%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,35%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (34%positive,ANone,36%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_cinfo_dref_off272)
             s) <> (eval (ENum (0)) s))%Z)),40%positive)::
             (38%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_cinfo_dref_off272)
             s) = (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,45%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (41%positive,ANone,43%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AAssign IDencode_mcu_AC_refine_temp None),
             48%positive)::(48%positive,AWeaken,49%positive)::
             (49%positive,ANone,75%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_r) s) >
             (eval (ENum (15)) s))%Z)),54%positive)::
             (52%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_r) s) <=
             (eval (ENum (15)) s))%Z)),53%positive)::
             (53%positive,AWeaken,59%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_k) s) <=
             (eval (EVar IDencode_mcu_AC_refine_EOB) s))%Z)),68%positive)::
             (57%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_k) s) >
             (eval (EVar IDencode_mcu_AC_refine_EOB) s))%Z)),58%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_temp) s) >
             (eval (ENum (1)) s))%Z)),65%positive)::
             (59%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_temp) s) <=
             (eval (ENum (1)) s))%Z)),60%positive)::
             (60%positive,AWeaken,61%positive)::
             (61%positive,(AAssign IDencode_mcu_AC_refine_temp None),
             62%positive)::
             (62%positive,(AAssign IDencode_mcu_AC_refine_BR
             (Some (ENum (0)))),63%positive)::
             (63%positive,(AAssign IDencode_mcu_AC_refine_r
             (Some (ENum (0)))),64%positive)::
             (64%positive,ANone,77%positive)::
             (65%positive,AWeaken,66%positive)::
             (66%positive,(AAssign IDencode_mcu_AC_refine_BR
             (Some (EAdd (EVar IDencode_mcu_AC_refine_BR) (ENum (1))))),
             67%positive)::(67%positive,ANone,77%positive)::
             (68%positive,AWeaken,69%positive)::
             (69%positive,(AAssign IDencode_mcu_AC_refine_r
             (Some (ESub (EVar IDencode_mcu_AC_refine_r) (ENum (16))))),
             70%positive)::
             (70%positive,(AAssign IDencode_mcu_AC_refine_BR
             (Some (ENum (0)))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDencode_mcu_AC_refine_z
             (Some (EAdd (ENum (1)) (EVar IDencode_mcu_AC_refine_z)))),
             74%positive)::(74%positive,AWeaken,52%positive)::
             (75%positive,(AAssign IDencode_mcu_AC_refine_r
             (Some (EAdd (EVar IDencode_mcu_AC_refine_r) (ENum (1))))),
             76%positive)::(76%positive,ANone,77%positive)::
             (77%positive,(AAssign IDencode_mcu_AC_refine_k
             (Some (EAdd (EVar IDencode_mcu_AC_refine_k) (ENum (1))))),
             78%positive)::(78%positive,ANone,79%positive)::
             (79%positive,ANone,80%positive)::
             (80%positive,(AAssign IDencode_mcu_AC_refine_z
             (Some (EAdd (ENum (1)) (EVar IDencode_mcu_AC_refine_z)))),
             81%positive)::(81%positive,AWeaken,24%positive)::
             (82%positive,AWeaken,83%positive)::
             (83%positive,(AAssign IDencode_mcu_AC_refine_temp None),
             84%positive)::(84%positive,AWeaken,85%positive)::
             (85%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_temp) s) <
             (eval (ENum (0)) s))%Z)),87%positive)::
             (85%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_temp) s) >=
             (eval (ENum (0)) s))%Z)),86%positive)::
             (86%positive,AWeaken,90%positive)::
             (87%positive,AWeaken,88%positive)::
             (88%positive,(AAssign IDencode_mcu_AC_refine_temp
             (Some (ESub (ENum (0)) (EVar IDencode_mcu_AC_refine_temp)))),
             89%positive)::(89%positive,ANone,90%positive)::
             (90%positive,(AAssign IDencode_mcu_AC_refine_temp None),
             91%positive)::(91%positive,AWeaken,92%positive)::
             (92%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_temp) s) =
             (eval (ENum (1)) s))%Z)),94%positive)::
             (92%positive,(AGuard
             (fun s => ((eval (EVar IDencode_mcu_AC_refine_temp) s) <>
             (eval (ENum (1)) s))%Z)),93%positive)::
             (93%positive,AWeaken,97%positive)::
             (94%positive,AWeaken,95%positive)::
             (95%positive,(AAssign IDencode_mcu_AC_refine_EOB
             (Some (EVar IDencode_mcu_AC_refine_k))),96%positive)::
             (96%positive,ANone,97%positive)::
             (97%positive,ANone,98%positive)::
             (98%positive,(AAssign IDencode_mcu_AC_refine_k
             (Some (EAdd (EVar IDencode_mcu_AC_refine_k) (ENum (1))))),
             99%positive)::(99%positive,ANone,100%positive)::
             (100%positive,ANone,101%positive)::
             (101%positive,(AAssign IDencode_mcu_AC_refine_z
             (Some (EAdd (ENum (1)) (EVar IDencode_mcu_AC_refine_z)))),
             102%positive)::(102%positive,AWeaken,17%positive)::nil
|}.

Definition encode_mcu_AC_refine_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 3%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 4%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 5%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 6%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 7%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 8%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_cinfo_dref_off272) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_cinfo_dref_off272) <= 0)%Z
    | 9%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 10%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 11%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 12%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 13%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 14%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_EOB) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB) <= 0)%Z
    | 15%positive => (-1 * (s IDencode_mcu_AC_refine_EOB) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_EOB) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 16%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_EOB) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB) <= 0)%Z
    | 17%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 18%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0)%Z
    | 19%positive => (1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 20%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 21%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 22%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 23%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 24%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 25%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 27%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 28%positive => (1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 29%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 30%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 33%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 34%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 35%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 36%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 37%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 38%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 39%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_cinfo_dref_off272) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_cinfo_dref_off272) <= 0)%Z
    | 40%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 41%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 42%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 43%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 44%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 45%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_Se)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 46%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 47%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 48%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 49%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 50%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 51%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 52%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 53%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) + -15 <= 0)%Z
    | 54%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0)%Z
    | 55%positive => (-1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 56%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0)%Z
    | 57%positive => (-1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 58%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_EOB)+ -1 * (s IDencode_mcu_AC_refine_k) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 60%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_temp) + -1 <= 0)%Z
    | 61%positive => (1 * (s IDencode_mcu_AC_refine_temp) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 62%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 63%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 64%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 65%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_temp) + 2 <= 0)%Z
    | 66%positive => (-1 * (s IDencode_mcu_AC_refine_temp) + 2 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 67%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_temp) + 2 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) + 1 <= 0)%Z
    | 68%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 69%positive => (-1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 16 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 70%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 71%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 72%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0)%Z
    | 73%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 74%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_EOB)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) + 1 <= 0)%Z
    | 75%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 76%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) + 1 <= 0)%Z
    | 77%positive => (-1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 78%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0)%Z
    | 79%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 80%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0)%Z
    | 81%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_r) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) + 1 <= 0)%Z
    | 82%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 83%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 84%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 85%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 86%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_temp) <= 0)%Z
    | 87%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_temp) + 1 <= 0)%Z
    | 88%positive => (1 * (s IDencode_mcu_AC_refine_temp) + 1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 89%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_temp) + 1 <= 0)%Z
    | 90%positive => (-1 * (s IDencode_mcu_AC_refine_temp) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 91%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 92%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 93%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 94%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_temp) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_temp) + 1 <= 0)%Z
    | 95%positive => (-1 * (s IDencode_mcu_AC_refine_temp) + 1 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_temp) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 96%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ 1 * (s IDencode_mcu_AC_refine_temp) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_temp) + 1 <= 0 /\ 1 * (s IDencode_mcu_AC_refine_EOB)+ -1 * (s IDencode_mcu_AC_refine_Se) <= 0)%Z
    | 97%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0)%Z
    | 98%positive => (-1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) <= 0)%Z
    | 99%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0)%Z
    | 100%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) <= 0)%Z
    | 101%positive => (-1 * (s IDencode_mcu_AC_refine_z) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0)%Z
    | 102%positive => (-1 * (s IDencode_mcu_AC_refine_Se)+ 1 * (s IDencode_mcu_AC_refine_k) + -1 <= 0 /\ -1 * (s IDencode_mcu_AC_refine_BR) <= 0 /\ -1 * (s IDencode_mcu_AC_refine_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition encode_mcu_AC_refine_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((33 # 16) * max0(1
                                      - (s IDencode_mcu_AC_refine_cinfo_dref_off404)
                                      + (s IDencode_mcu_AC_refine_cinfo_dref_off408)))%Q
    | 2%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)
                                        + (s IDencode_mcu_AC_refine_cinfo_dref_off408)))%Q
    | 3%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)
                                        + (s IDencode_mcu_AC_refine_cinfo_dref_off408)))%Q
    | 4%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)
                                        + (s IDencode_mcu_AC_refine_cinfo_dref_off408)))%Q
    | 5%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 6%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 7%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 8%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 9%positive => ((s IDencode_mcu_AC_refine_z)
                     + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                        - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 10%positive => ((s IDencode_mcu_AC_refine_z)
                      + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 11%positive => ((s IDencode_mcu_AC_refine_z)
                      + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 12%positive => ((s IDencode_mcu_AC_refine_z)
                      + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 13%positive => ((s IDencode_mcu_AC_refine_z)
                      + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 14%positive => ((s IDencode_mcu_AC_refine_z)
                      + (33 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404)))%Q
    | 15%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 16%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 17%positive => ((17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                       - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 18%positive => ((17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                       - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 19%positive => ((17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                       - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 20%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 21%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 22%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 23%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 24%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 25%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 26%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 27%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 28%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 29%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 30%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 31%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 32%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 33%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 34%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 35%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 36%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 37%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 38%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 39%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 40%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 41%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 42%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 43%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 44%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z))%Q
    | 45%positive => ((s IDencode_mcu_AC_refine_z))%Q
    | 46%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 47%positive => (-(17 # 16) * (s IDencode_mcu_AC_refine_Se)
                      + (17 # 16) * (s IDencode_mcu_AC_refine_k)
                      + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k))
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 48%positive => (-(17 # 16) * (s IDencode_mcu_AC_refine_Se)
                      + (17 # 16) * (s IDencode_mcu_AC_refine_k)
                      + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k))
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 49%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 50%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 51%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 52%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 53%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 54%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 55%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 56%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 57%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 58%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 59%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 60%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 61%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 62%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 63%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 64%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 65%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 66%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 67%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 68%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 69%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 70%positive => ((33 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 71%positive => ((33 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 72%positive => ((33 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 73%positive => ((33 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 74%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 75%positive => ((17 # 16) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 76%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 77%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0((s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 78%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 79%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 80%positive => ((1 # 1) + (1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 81%positive => ((1 # 16) * (s IDencode_mcu_AC_refine_r)
                      + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_k)))%Q
    | 82%positive => ((17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                       - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 83%positive => ((17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                       - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 84%positive => ((17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                       - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k))
                      + max0((s IDencode_mcu_AC_refine_z)))%Q
    | 85%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 86%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 87%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 88%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 89%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 90%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 91%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 92%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 93%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 94%positive => ((s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 95%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 96%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 97%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 98%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0((s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 99%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                      + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                         - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                      + max0(1 + (s IDencode_mcu_AC_refine_Se)
                             - (s IDencode_mcu_AC_refine_k)))%Q
    | 100%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                       + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                          - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                       + max0(1 + (s IDencode_mcu_AC_refine_Se)
                              - (s IDencode_mcu_AC_refine_k)))%Q
    | 101%positive => ((1 # 1) + (s IDencode_mcu_AC_refine_z)
                       + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                          - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                       + max0(1 + (s IDencode_mcu_AC_refine_Se)
                              - (s IDencode_mcu_AC_refine_k)))%Q
    | 102%positive => ((s IDencode_mcu_AC_refine_z)
                       + (17 # 16) * max0(1 + (s IDencode_mcu_AC_refine_Se)
                                          - (s IDencode_mcu_AC_refine_cinfo_dref_off404))
                       + max0(1 + (s IDencode_mcu_AC_refine_Se)
                              - (s IDencode_mcu_AC_refine_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition encode_mcu_AC_refine_hints (p : node) (s : state) := 
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
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDencode_mcu_AC_refine_z)) (0))) (F_max0_ge_0 ((s IDencode_mcu_AC_refine_z)))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDencode_mcu_AC_refine_Se)
                                                             - (s IDencode_mcu_AC_refine_k)) ((s IDencode_mcu_AC_refine_Se)
                                                                    - (s IDencode_mcu_AC_refine_k)));
                      (*-1 0*) F_max0_ge_0 ((s IDencode_mcu_AC_refine_Se)
                                            - (s IDencode_mcu_AC_refine_k))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDencode_mcu_AC_refine_z))) (F_check_ge ((s IDencode_mcu_AC_refine_z)) (0))]
    | 24%positive => []
    | 25%positive => [(*0 1.0625*) F_max0_monotonic (F_check_ge (1
                                                                 + (s IDencode_mcu_AC_refine_Se)
                                                                 - (s IDencode_mcu_AC_refine_k)) ((s IDencode_mcu_AC_refine_Se)
                                                                    - (s IDencode_mcu_AC_refine_k)))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1.0625 0*) F_max0_ge_0 ((s IDencode_mcu_AC_refine_Se)
                                                 - (s IDencode_mcu_AC_refine_k))]
    | 30%positive => [(*-1.0625 0*) F_max0_ge_0 ((s IDencode_mcu_AC_refine_Se)
                                                 - (s IDencode_mcu_AC_refine_k))]
    | 31%positive => [(*-1.0625 0*) F_max0_ge_0 ((s IDencode_mcu_AC_refine_Se)
                                                 - (s IDencode_mcu_AC_refine_k))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*-0.0625 0*) F_max0_monotonic (F_check_ge ((s IDencode_mcu_AC_refine_r)) (-16
                                                                    + (s IDencode_mcu_AC_refine_r)));
                      (*-0.0625 0*) F_max0_ge_0 (-16
                                                 + (s IDencode_mcu_AC_refine_r));
                      (*-0.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDencode_mcu_AC_refine_r)) (0))) (F_max0_ge_0 ((s IDencode_mcu_AC_refine_r)))]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-0.0625 0*) F_max0_monotonic (F_check_ge ((s IDencode_mcu_AC_refine_r)) (-16
                                                                    + (s IDencode_mcu_AC_refine_r)));
                      (*-0.0625 0*) F_max0_ge_0 (-16
                                                 + (s IDencode_mcu_AC_refine_r));
                      (*-0.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDencode_mcu_AC_refine_r)) (0))) (F_max0_ge_0 ((s IDencode_mcu_AC_refine_r)))]
    | 45%positive => []
    | 46%positive => [(*-1.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDencode_mcu_AC_refine_Se)
                                                                    - (s IDencode_mcu_AC_refine_k)) (0))) (F_max0_ge_0 ((s IDencode_mcu_AC_refine_Se)
                                                                    - (s IDencode_mcu_AC_refine_k)))]
    | 47%positive => []
    | 48%positive => [(*-1.0625 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDencode_mcu_AC_refine_Se)
                                                                    - (s IDencode_mcu_AC_refine_k))) (F_check_ge (1
                                                                    + (s IDencode_mcu_AC_refine_Se)
                                                                    - (s IDencode_mcu_AC_refine_k)) (0))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-0.0625 0*) F_one]
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => [(*-0.0625 0*) F_one]
    | 59%positive => []
    | 60%positive => [(*-0.0625 0*) F_max0_monotonic (F_check_ge ((s IDencode_mcu_AC_refine_r)) (-16
                                                                    + (s IDencode_mcu_AC_refine_r)));
                      (*-0.0625 0*) F_max0_ge_0 (-16
                                                 + (s IDencode_mcu_AC_refine_r));
                      (*-0.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDencode_mcu_AC_refine_r)) (0))) (F_max0_ge_0 ((s IDencode_mcu_AC_refine_r)))]
    | 61%positive => []
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
    | 84%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDencode_mcu_AC_refine_z))) (F_check_ge ((s IDencode_mcu_AC_refine_z)) (0))]
    | 85%positive => []
    | 86%positive => []
    | 87%positive => []
    | 88%positive => []
    | 89%positive => []
    | 90%positive => []
    | 91%positive => []
    | 92%positive => []
    | 93%positive => [(*0 1*) F_max0_pre_decrement (1
                                                    + (s IDencode_mcu_AC_refine_Se)
                                                    - (s IDencode_mcu_AC_refine_k)) (1)]
    | 94%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDencode_mcu_AC_refine_Se)
                                                     - (s IDencode_mcu_AC_refine_k)) (1)]
    | 95%positive => []
    | 96%positive => []
    | 97%positive => []
    | 98%positive => []
    | 99%positive => []
    | 100%positive => []
    | 101%positive => []
    | 102%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDencode_mcu_AC_refine_z)) (0))) (F_max0_ge_0 ((s IDencode_mcu_AC_refine_z)))]
    | _ => []
  end.


Theorem encode_mcu_AC_refine_ai_correct:
  forall s p' s', steps (g_start encode_mcu_AC_refine) s (g_edges encode_mcu_AC_refine) p' s' -> encode_mcu_AC_refine_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem encode_mcu_AC_refine_pot_correct:
  forall s p' s',
    steps (g_start encode_mcu_AC_refine) s (g_edges encode_mcu_AC_refine) p' s' ->
    (encode_mcu_AC_refine_pot (g_start encode_mcu_AC_refine) s >= encode_mcu_AC_refine_pot p' s')%Q.
Proof.
  check_lp encode_mcu_AC_refine_ai_correct encode_mcu_AC_refine_hints.
Qed.

