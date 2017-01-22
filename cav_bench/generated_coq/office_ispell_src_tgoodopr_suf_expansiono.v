Require Import pasta.Pasta.

Notation IDpr_suf_expansion_z := 1%positive.
Notation IDpr_suf_expansion__tmp := 2%positive.
Notation IDpr_suf_expansion__tmp1 := 3%positive.
Notation IDpr_suf_expansion_cond := 4%positive.
Notation IDpr_suf_expansion_flent_dref_off18 := 5%positive.
Notation IDpr_suf_expansion_flent_dref_off20 := 6%positive.
Notation IDpr_suf_expansion_flent_dref_off22 := 7%positive.
Notation IDpr_suf_expansion_tlen := 8%positive.
Notation IDpr_suf_expansion_croot := 9%positive.
Notation IDpr_suf_expansion_extra := 10%positive.
Notation IDpr_suf_expansion_flent := 11%positive.
Notation IDpr_suf_expansion_option := 12%positive.
Notation IDpr_suf_expansion_rootword := 13%positive.
Definition pr_suf_expansion : graph := {|
  g_start := 1%positive;
  g_end := 52%positive;
  g_edges := (1%positive,(AAssign IDpr_suf_expansion_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDpr_suf_expansion__tmp1
             (Some (EVar IDpr_suf_expansion_option))),3%positive)::
             (3%positive,(AAssign IDpr_suf_expansion_tlen None),4%positive)::
             (4%positive,(AAssign IDpr_suf_expansion_cond
             (Some (EVar IDpr_suf_expansion_flent_dref_off22))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion_cond) s) >
             (eval (EVar IDpr_suf_expansion_tlen) s))%Z)),48%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion_cond) s) <=
             (eval (EVar IDpr_suf_expansion_tlen) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDpr_suf_expansion_tlen)
             (EVar IDpr_suf_expansion_flent_dref_off18)) s) <=
             (eval (ENum (0)) s))%Z)),44%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (ESub (EVar IDpr_suf_expansion_tlen)
             (EVar IDpr_suf_expansion_flent_dref_off18)) s) >
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDpr_suf_expansion_cond
             (Some (EAdd (EVar IDpr_suf_expansion_cond) (ENum (-1))))),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDpr_suf_expansion_cond)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),36%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDpr_suf_expansion_cond)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion_flent_dref_off20)
             s) <> (eval (ENum (0)) s))%Z)),19%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion_flent_dref_off20) s) =
             (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,24%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,22%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion__tmp1) s) =
             (eval (ENum (3)) s))%Z)),26%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion__tmp1) s) <>
             (eval (ENum (3)) s))%Z)),25%positive)::
             (25%positive,AWeaken,29%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion__tmp1) s) <>
             (eval (ENum (4)) s))%Z)),31%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EVar IDpr_suf_expansion__tmp1) s) =
             (eval (ENum (4)) s))%Z)),30%positive)::
             (30%positive,AWeaken,33%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDpr_suf_expansion__tmp
             (Some (ESub (EAdd (EVar IDpr_suf_expansion_tlen)
             (EVar IDpr_suf_expansion_flent_dref_off20))
             (EVar IDpr_suf_expansion_flent_dref_off18)))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,52%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,41%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDpr_suf_expansion_z
             (Some (EAdd (ENum (1)) (EVar IDpr_suf_expansion_z)))),
             11%positive)::
             (41%positive,(AAssign IDpr_suf_expansion__tmp
             (Some (ENum (0)))),42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,AWeaken,52%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDpr_suf_expansion__tmp
             (Some (ENum (0)))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,52%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDpr_suf_expansion__tmp
             (Some (ENum (0)))),50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,52%positive)::nil
|}.

Definition pr_suf_expansion_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 4%positive => (1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 5%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 6%positive => (1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0)%Z
    | 8%positive => (1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 10%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 11%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 13%positive => (1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 15%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 16%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off20) <= 0 /\ -1 * (s IDpr_suf_expansion_flent_dref_off20) <= 0)%Z
    | 17%positive => (-1 * (s IDpr_suf_expansion_flent_dref_off20) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off20) <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off20) <= 0 /\ -1 * (s IDpr_suf_expansion_flent_dref_off20) <= 0)%Z
    | 19%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 20%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 22%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 23%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 24%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 25%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 26%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp1) + -3 <= 0 /\ -1 * (s IDpr_suf_expansion__tmp1) + 3 <= 0)%Z
    | 27%positive => (-1 * (s IDpr_suf_expansion__tmp1) + 3 <= 0 /\ 1 * (s IDpr_suf_expansion__tmp1) + -3 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp1) + -3 <= 0 /\ -1 * (s IDpr_suf_expansion__tmp1) + 3 <= 0)%Z
    | 29%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp1) + -4 <= 0 /\ -1 * (s IDpr_suf_expansion__tmp1) + 4 <= 0)%Z
    | 31%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 32%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 34%positive => (1 * (s IDpr_suf_expansion_cond) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond) <= 0)%Z
    | 36%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_cond) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDpr_suf_expansion_cond) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_cond) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDpr_suf_expansion_cond) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 40%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_cond) + 1 <= 0)%Z
    | 41%positive => (1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_cond) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDpr_suf_expansion_cond) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion__tmp) <= 0 /\ -1 * (s IDpr_suf_expansion__tmp) <= 0)%Z
    | 43%positive => (-1 * (s IDpr_suf_expansion__tmp) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp) <= 0 /\ 1 * (s IDpr_suf_expansion_flent_dref_off18)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ -1 * (s IDpr_suf_expansion_cond) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ -1 * (s IDpr_suf_expansion_flent_dref_off18)+ 1 * (s IDpr_suf_expansion_tlen) <= 0)%Z
    | 45%positive => (-1 * (s IDpr_suf_expansion_flent_dref_off18)+ 1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 46%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ -1 * (s IDpr_suf_expansion_flent_dref_off18)+ 1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp) <= 0 /\ -1 * (s IDpr_suf_expansion__tmp) <= 0)%Z
    | 47%positive => (-1 * (s IDpr_suf_expansion__tmp) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp) <= 0 /\ -1 * (s IDpr_suf_expansion_flent_dref_off18)+ 1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_cond)+ -1 * (s IDpr_suf_expansion_tlen) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 48%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_cond)+ 1 * (s IDpr_suf_expansion_tlen) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDpr_suf_expansion_cond)+ 1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 50%positive => (-1 * (s IDpr_suf_expansion_z) <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_cond)+ 1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion__tmp) <= 0 /\ -1 * (s IDpr_suf_expansion__tmp) <= 0)%Z
    | 51%positive => (-1 * (s IDpr_suf_expansion__tmp) <= 0 /\ 1 * (s IDpr_suf_expansion__tmp) <= 0 /\ -1 * (s IDpr_suf_expansion_cond)+ 1 * (s IDpr_suf_expansion_tlen) + 1 <= 0 /\ 1 * (s IDpr_suf_expansion_z) <= 0 /\ -1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | 52%positive => (-1 * (s IDpr_suf_expansion_z) <= 0)%Z
    | _ => False
  end.

Definition pr_suf_expansion_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpr_suf_expansion_flent_dref_off22)))%Q
    | 2%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_flent_dref_off22)))%Q
    | 3%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_flent_dref_off22)))%Q
    | 4%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_flent_dref_off22)))%Q
    | 5%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_cond)))%Q
    | 6%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_cond)))%Q
    | 7%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_cond)))%Q
    | 8%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_cond)))%Q
    | 9%positive => ((s IDpr_suf_expansion_z)
                     + max0((s IDpr_suf_expansion_cond)))%Q
    | 10%positive => ((s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 11%positive => ((s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 12%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 13%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 14%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 15%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 16%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 17%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 18%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 19%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 20%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 21%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 22%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 23%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 24%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 25%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 26%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 27%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 28%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 29%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 30%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 31%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 32%positive => ((s IDpr_suf_expansion_z))%Q
    | 33%positive => ((s IDpr_suf_expansion_z))%Q
    | 34%positive => ((s IDpr_suf_expansion_z))%Q
    | 35%positive => ((s IDpr_suf_expansion_z))%Q
    | 36%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 37%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 38%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 39%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 40%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 41%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 42%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 43%positive => ((1 # 1) + (s IDpr_suf_expansion_z)
                      + max0(-1 + (s IDpr_suf_expansion_cond)))%Q
    | 44%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 45%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 46%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 47%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 48%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 49%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 50%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 51%positive => ((s IDpr_suf_expansion_z)
                      + max0((s IDpr_suf_expansion_cond)))%Q
    | 52%positive => ((s IDpr_suf_expansion_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pr_suf_expansion_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDpr_suf_expansion_cond)) (-1
                                                                    + (s IDpr_suf_expansion_cond)))]
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
    | 30%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDpr_suf_expansion_cond)) (-1
                                                                    + (s IDpr_suf_expansion_cond)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpr_suf_expansion_cond))]
    | 31%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDpr_suf_expansion_cond)) (-1
                                                                    + (s IDpr_suf_expansion_cond)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpr_suf_expansion_cond))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_max0_pre_decrement ((s IDpr_suf_expansion_cond)) (1)]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpr_suf_expansion_cond))]
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpr_suf_expansion_cond)) (-1
                                                                    + (s IDpr_suf_expansion_cond)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpr_suf_expansion_cond))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpr_suf_expansion_cond)) (-1
                                                                    + (s IDpr_suf_expansion_cond)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpr_suf_expansion_cond))]
    | 52%positive => []
    | _ => []
  end.


Theorem pr_suf_expansion_ai_correct:
  forall s p' s', steps (g_start pr_suf_expansion) s (g_edges pr_suf_expansion) p' s' -> pr_suf_expansion_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pr_suf_expansion_pot_correct:
  forall s p' s',
    steps (g_start pr_suf_expansion) s (g_edges pr_suf_expansion) p' s' ->
    (pr_suf_expansion_pot (g_start pr_suf_expansion) s >= pr_suf_expansion_pot p' s')%Q.
Proof.
  check_lp pr_suf_expansion_ai_correct pr_suf_expansion_hints.
Qed.

