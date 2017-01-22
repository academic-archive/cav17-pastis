Require Import pasta.Pasta.

Notation IDfont_dir_enum_ptrs_z := 1%positive.
Notation IDfont_dir_enum_ptrs__tmp := 2%positive.
Notation IDfont_dir_enum_ptrs__tmp1 := 3%positive.
Notation IDfont_dir_enum_ptrs_cci := 4%positive.
Notation IDfont_dir_enum_ptrs_count := 5%positive.
Notation IDfont_dir_enum_ptrs_offset := 6%positive.
Notation IDfont_dir_enum_ptrs_tmask := 7%positive.
Notation IDfont_dir_enum_ptrs_vptr_dref_off128 := 8%positive.
Notation IDfont_dir_enum_ptrs_vptr_dref_off132 := 9%positive.
Notation IDfont_dir_enum_ptrs_vptr_dref_off48_off40 := 10%positive.
Notation IDfont_dir_enum_ptrs_index := 11%positive.
Notation IDfont_dir_enum_ptrs_pep := 12%positive.
Notation IDfont_dir_enum_ptrs_size := 13%positive.
Notation IDfont_dir_enum_ptrs_vptr := 14%positive.
Definition font_dir_enum_ptrs : graph := {|
  g_start := 1%positive;
  g_end := 57%positive;
  g_edges := (1%positive,(AAssign IDfont_dir_enum_ptrs_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_tmask) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_offset) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDfont_dir_enum_ptrs__tmp1
             (Some (EVar IDfont_dir_enum_ptrs_size))),6%positive)::
             (6%positive,(AAssign IDfont_dir_enum_ptrs__tmp
             (Some (EVar IDfont_dir_enum_ptrs_index))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,21%positive)::(8%positive,ANone,18%positive)::
             (8%positive,ANone,15%positive)::(8%positive,ANone,12%positive)::
             (8%positive,ANone,9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,57%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,57%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,57%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,57%positive)::
             (21%positive,(AAssign IDfont_dir_enum_ptrs_cci
             (Some (ESub (EVar IDfont_dir_enum_ptrs__tmp) (ENum (4))))),
             22%positive)::
             (22%positive,(AAssign IDfont_dir_enum_ptrs_tmask
             (Some (EVar IDfont_dir_enum_ptrs_vptr_dref_off48_off40))),
             23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_cci) s) =
             (eval (ENum (0)) s))%Z)),36%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_cci) s) <>
             (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_cci) s) =
             (eval (EAdd (EVar IDfont_dir_enum_ptrs_vptr_dref_off128)
             (ENum (1))) s))%Z)),31%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_cci) s) <>
             (eval (EAdd (EVar IDfont_dir_enum_ptrs_vptr_dref_off128)
             (ENum (1))) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDfont_dir_enum_ptrs_offset
             (Some (ENum (0)))),29%positive)::
             (29%positive,(AAssign IDfont_dir_enum_ptrs_count
             (Some (EVar IDfont_dir_enum_ptrs_cci))),30%positive)::
             (30%positive,ANone,35%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AAssign IDfont_dir_enum_ptrs_offset
             (Some (EAdd (EVar IDfont_dir_enum_ptrs_vptr_dref_off132)
             (ENum (1))))),33%positive)::
             (33%positive,(AAssign IDfont_dir_enum_ptrs_count
             (Some (ENum (1)))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,40%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,(AAssign IDfont_dir_enum_ptrs_offset
             (Some (ENum (0)))),38%positive)::
             (38%positive,(AAssign IDfont_dir_enum_ptrs_count
             (Some (ENum (1)))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_offset) s) <=
             (eval (EVar IDfont_dir_enum_ptrs_tmask) s))%Z)),46%positive)::
             (42%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_enum_ptrs_offset) s) >
             (eval (EVar IDfont_dir_enum_ptrs_tmask) s))%Z)),43%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,57%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,48%positive)::
             (47%positive,ANone,59%positive)::
             (48%positive,(AAssign IDfont_dir_enum_ptrs_count
             (Some (EAdd (EVar IDfont_dir_enum_ptrs_count) (ENum (-1))))),
             49%positive)::(49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDfont_dir_enum_ptrs_count)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),58%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDfont_dir_enum_ptrs_count)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,(AAssign IDfont_dir_enum_ptrs_vptr_dref_off128
             (Some (EVar IDfont_dir_enum_ptrs_cci))),53%positive)::
             (53%positive,(AAssign IDfont_dir_enum_ptrs_vptr_dref_off132
             (Some (EVar IDfont_dir_enum_ptrs_offset))),54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,AWeaken,57%positive)::
             (58%positive,AWeaken,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDfont_dir_enum_ptrs_offset
             (Some (EAdd (EVar IDfont_dir_enum_ptrs_offset) (ENum (1))))),
             61%positive)::(61%positive,ANone,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,(AAssign IDfont_dir_enum_ptrs_z
             (Some (EAdd (ENum (1)) (EVar IDfont_dir_enum_ptrs_z)))),
             64%positive)::(64%positive,AWeaken,42%positive)::nil
|}.

Definition font_dir_enum_ptrs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 4%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 5%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 6%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 7%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 8%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 9%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 10%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 11%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 12%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 13%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 14%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 15%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 16%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 17%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 18%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 19%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 20%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 21%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 22%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 23%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 24%positive => (1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 25%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 26%positive => (1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 27%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 28%positive => (1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 29%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 30%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 31%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci)+ -1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_cci)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDfont_dir_enum_ptrs_cci)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + 1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci)+ -1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + -1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 33%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci)+ -1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_cci)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDfont_dir_enum_ptrs_cci)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + 1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci)+ -1 * (s IDfont_dir_enum_ptrs_vptr_dref_off128) + -1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 36%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_cci) <= 0)%Z
    | 37%positive => (-1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 38%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset) <= 0)%Z
    | 39%positive => (-1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_cci) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 41%positive => (1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 42%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 43%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset)+ 1 * (s IDfont_dir_enum_ptrs_tmask) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDfont_dir_enum_ptrs_offset)+ 1 * (s IDfont_dir_enum_ptrs_tmask) + 1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 45%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_offset)+ 1 * (s IDfont_dir_enum_ptrs_tmask) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 47%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 48%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 49%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 50%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 51%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 53%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off132) <= 0)%Z
    | 55%positive => (-1 * (s IDfont_dir_enum_ptrs_tmask)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off132) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDfont_dir_enum_ptrs_count) + 1 <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_count) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_tmask)+ 1 * (s IDfont_dir_enum_ptrs_vptr_dref_off132) <= 0)%Z
    | 57%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 58%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 59%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0)%Z
    | 60%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 61%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) + -1 <= 0)%Z
    | 62%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) <= 0)%Z
    | 63%positive => (-1 * (s IDfont_dir_enum_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) + -1 <= 0)%Z
    | 64%positive => (1 * (s IDfont_dir_enum_ptrs_offset)+ -1 * (s IDfont_dir_enum_ptrs_tmask) + -1 <= 0 /\ -1 * (s IDfont_dir_enum_ptrs_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition font_dir_enum_ptrs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 2%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 3%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 4%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 5%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 6%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 7%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 8%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 9%positive => ((s IDfont_dir_enum_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                     + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 10%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 11%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 12%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 13%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 14%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 15%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 16%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 17%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 18%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 19%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 20%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 21%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 22%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))
                      + max0(-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40)))%Q
    | 23%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 24%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 25%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 26%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 27%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 28%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 29%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 30%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 31%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 32%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 33%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 34%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 35%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 36%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask))
                      + max0((s IDfont_dir_enum_ptrs_tmask)
                             - (s IDfont_dir_enum_ptrs_vptr_dref_off132)))%Q
    | 37%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 38%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 39%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 40%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 41%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 42%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 43%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 44%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 45%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 46%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 47%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 48%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 49%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 50%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 51%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 52%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 53%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_cci)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off128))
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 54%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_cci)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off128))
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 55%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_cci)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off128))
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 56%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_cci)
                             + (s IDfont_dir_enum_ptrs_vptr_dref_off128))
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 57%positive => ((s IDfont_dir_enum_ptrs_z))%Q
    | 58%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 59%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 60%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(-(s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 61%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 62%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 63%positive => ((1 # 1) + (s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | 64%positive => ((s IDfont_dir_enum_ptrs_z)
                      + max0(1 - (s IDfont_dir_enum_ptrs_offset)
                             + (s IDfont_dir_enum_ptrs_tmask)))%Q
    | _ => (0 # 1)%Q
  end.

Definition font_dir_enum_ptrs_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_ge_0 (1
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40));
                      (*-1 0*) F_max0_ge_0 (-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_ge_0 (1
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40));
                      (*-1 0*) F_max0_ge_0 (-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_ge_0 (1
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40));
                      (*-1 0*) F_max0_ge_0 (-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_ge_0 (1
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40));
                      (*-1 0*) F_max0_ge_0 (-(s IDfont_dir_enum_ptrs_vptr_dref_off132)
                                            + (s IDfont_dir_enum_ptrs_vptr_dref_off48_off40))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*0 1*) F_max0_ge_0 ((s IDfont_dir_enum_ptrs_tmask)
                                           - (s IDfont_dir_enum_ptrs_vptr_dref_off132))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_ge_0 (1
                                            + (s IDfont_dir_enum_ptrs_tmask))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_max0_ge_0 ((s IDfont_dir_enum_ptrs_tmask)
                                            - (s IDfont_dir_enum_ptrs_vptr_dref_off132))]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             - (s IDfont_dir_enum_ptrs_offset)
                                                             + (s IDfont_dir_enum_ptrs_tmask)) (-
                                                                    (s IDfont_dir_enum_ptrs_offset)
                                                                    + (s IDfont_dir_enum_ptrs_tmask)));
                      (*-1 0*) F_max0_ge_0 (-(s IDfont_dir_enum_ptrs_offset)
                                            + (s IDfont_dir_enum_ptrs_tmask))]
    | 46%positive => [(*0 1*) F_max0_pre_decrement (1
                                                    - (s IDfont_dir_enum_ptrs_offset)
                                                    + (s IDfont_dir_enum_ptrs_tmask)) (1)]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => [(*-1 0*) F_max0_ge_0 (-(s IDfont_dir_enum_ptrs_offset)
                                            + (s IDfont_dir_enum_ptrs_tmask));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDfont_dir_enum_ptrs_cci)
                                                                 + (s IDfont_dir_enum_ptrs_vptr_dref_off128))) (F_check_ge (0) (0))]
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | _ => []
  end.


Theorem font_dir_enum_ptrs_ai_correct:
  forall s p' s', steps (g_start font_dir_enum_ptrs) s (g_edges font_dir_enum_ptrs) p' s' -> font_dir_enum_ptrs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem font_dir_enum_ptrs_pot_correct:
  forall s p' s',
    steps (g_start font_dir_enum_ptrs) s (g_edges font_dir_enum_ptrs) p' s' ->
    (font_dir_enum_ptrs_pot (g_start font_dir_enum_ptrs) s >= font_dir_enum_ptrs_pot p' s')%Q.
Proof.
  check_lp font_dir_enum_ptrs_ai_correct font_dir_enum_ptrs_hints.
Qed.

