Require Import pasta.Pasta.

Notation IDIII_hybrid_z := 1%positive.
Notation IDIII_hybrid__tmp := 2%positive.
Notation IDIII_hybrid_b := 3%positive.
Notation IDIII_hybrid_bt := 4%positive.
Notation IDIII_hybrid_gr_info_dref_off16 := 5%positive.
Notation IDIII_hybrid_gr_info_dref_off20 := 6%positive.
Notation IDIII_hybrid_gr_info_dref_off64 := 7%positive.
Notation IDIII_hybrid_i := 8%positive.
Notation IDIII_hybrid_sb := 9%positive.
Notation IDIII_hybrid_ch := 10%positive.
Notation IDIII_hybrid_fsIn := 11%positive.
Notation IDIII_hybrid_gr_info := 12%positive.
Notation IDIII_hybrid_tsOut := 13%positive.
Definition III_hybrid : graph := {|
  g_start := 1%positive;
  g_end := 41%positive;
  g_edges := (1%positive,(AAssign IDIII_hybrid_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDIII_hybrid_gr_info_dref_off64) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDIII_hybrid__tmp
             (Some (EVar IDIII_hybrid_ch))),6%positive)::
             (6%positive,(AAssign IDIII_hybrid_sb (Some (ENum (0)))),
             7%positive)::
             (7%positive,(AAssign IDIII_hybrid_b None),8%positive)::
             (8%positive,(AAssign IDIII_hybrid_b (Some (EAdd (ESub (ENum (0))
             (EVar IDIII_hybrid_b)) (ENum (1))))),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDIII_hybrid_gr_info_dref_off20) s) <>
             (eval (ENum (0)) s))%Z)),12%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDIII_hybrid_gr_info_dref_off20) s) =
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,15%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDIII_hybrid_sb (Some (ENum (2)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDIII_hybrid_bt
             (Some (EVar IDIII_hybrid_gr_info_dref_off16))),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_bt)
             s) = (eval (ENum (2)) s))%Z)),31%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_bt)
             s) <> (eval (ENum (2)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) < (eval (EVar IDIII_hybrid_gr_info_dref_off64) s))%Z)),
             24%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) >= (eval (EVar IDIII_hybrid_gr_info_dref_off64) s))%Z)),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,ANone,37%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDIII_hybrid_sb
             (Some (EAdd (EVar IDIII_hybrid_sb) (ENum (2))))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDIII_hybrid_z (Some (EAdd (ENum (1))
             (EVar IDIII_hybrid_z)))),30%positive)::
             (30%positive,AWeaken,21%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) < (eval (EVar IDIII_hybrid_gr_info_dref_off64) s))%Z)),
             61%positive)::
             (34%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) >= (eval (EVar IDIII_hybrid_gr_info_dref_off64) s))%Z)),
             35%positive)::(35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) < (eval (ENum (32)) s))%Z)),42%positive)::
             (39%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_sb)
             s) >= (eval (ENum (32)) s))%Z)),40%positive)::
             (40%positive,AWeaken,41%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AAssign IDIII_hybrid_i (Some (ENum (0)))),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_i) s) <
             (eval (ENum (18)) s))%Z)),54%positive)::
             (46%positive,(AGuard (fun s => ((eval (EVar IDIII_hybrid_i)
             s) >= (eval (ENum (18)) s))%Z)),47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDIII_hybrid_sb
             (Some (EAdd (EVar IDIII_hybrid_sb) (ENum (1))))),50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDIII_hybrid_z (Some (EAdd (ENum (1))
             (EVar IDIII_hybrid_z)))),53%positive)::
             (53%positive,AWeaken,39%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDIII_hybrid_i
             (Some (EAdd (EVar IDIII_hybrid_i) (ENum (1))))),57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,(AAssign IDIII_hybrid_z (Some (EAdd (ENum (1))
             (EVar IDIII_hybrid_z)))),60%positive)::
             (60%positive,AWeaken,46%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,(AAssign IDIII_hybrid_sb
             (Some (EAdd (EVar IDIII_hybrid_sb) (ENum (2))))),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDIII_hybrid_z (Some (EAdd (ENum (1))
             (EVar IDIII_hybrid_z)))),67%positive)::
             (67%positive,AWeaken,34%positive)::nil
|}.

Definition III_hybrid_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0)%Z
    | 3%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 4%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 5%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 6%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 7%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 8%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 9%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 10%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 11%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off20) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off20) <= 0)%Z
    | 12%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 13%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 14%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0)%Z
    | 15%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 16%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 17%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 18%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 19%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 20%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 21%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 22%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 23%positive => (1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 24%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0)%Z
    | 26%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0)%Z
    | 28%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0)%Z
    | 29%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0)%Z
    | 30%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ -1 * (s IDIII_hybrid_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0)%Z
    | 32%positive => (-1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 33%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0)%Z
    | 34%positive => (-1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0)%Z
    | 35%positive => (1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 36%positive => (1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0)%Z
    | 37%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0)%Z
    | 38%positive => (1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 39%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 40%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) + 32 <= 0)%Z
    | 41%positive => (-1 * (s IDIII_hybrid_sb) + 32 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 42%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0)%Z
    | 43%positive => (1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 44%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ 1 * (s IDIII_hybrid_i) <= 0 /\ -1 * (s IDIII_hybrid_i) <= 0)%Z
    | 45%positive => (-1 * (s IDIII_hybrid_i) <= 0 /\ 1 * (s IDIII_hybrid_i) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0)%Z
    | 46%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_i) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0)%Z
    | 47%positive => (1 * (s IDIII_hybrid_i) + -18 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_i) + 18 <= 0)%Z
    | 48%positive => (-1 * (s IDIII_hybrid_i) + 18 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0)%Z
    | 49%positive => (1 * (s IDIII_hybrid_i) + -18 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_i) + 18 <= 0)%Z
    | 50%positive => (-1 * (s IDIII_hybrid_i) + 18 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -32 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -32 <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_i) + 18 <= 0)%Z
    | 52%positive => (-1 * (s IDIII_hybrid_i) + 18 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -32 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) + 1 <= 0)%Z
    | 53%positive => (1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -32 <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_i) + 18 <= 0 /\ -1 * (s IDIII_hybrid_z) + 1 <= 0)%Z
    | 54%positive => (1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_i) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_i) + -17 <= 0)%Z
    | 55%positive => (1 * (s IDIII_hybrid_i) + -17 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_i) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0)%Z
    | 56%positive => (1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_i) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_i) + -17 <= 0)%Z
    | 57%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_i) + 1 <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0)%Z
    | 58%positive => (1 * (s IDIII_hybrid_i) + -18 <= 0 /\ -1 * (s IDIII_hybrid_i) + 1 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0)%Z
    | 59%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_i) + 1 <= 0 /\ 1 * (s IDIII_hybrid_i) + -18 <= 0)%Z
    | 60%positive => (1 * (s IDIII_hybrid_i) + -18 <= 0 /\ -1 * (s IDIII_hybrid_i) + 1 <= 0 /\ 1 * (s IDIII_hybrid_sb) + -31 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ 1 * (s IDIII_hybrid_gr_info_dref_off64)+ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) + 1 <= 0)%Z
    | 61%positive => (1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + 1 <= 0)%Z
    | 62%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + 1 <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0)%Z
    | 63%positive => (1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_sb) <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + 1 <= 0)%Z
    | 64%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0)%Z
    | 65%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ -1 * (s IDIII_hybrid_z) <= 0)%Z
    | 66%positive => (-1 * (s IDIII_hybrid_z) <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ -1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0)%Z
    | 67%positive => (-1 * (s IDIII_hybrid_gr_info_dref_off64)+ 1 * (s IDIII_hybrid_sb) + -1 <= 0 /\ -1 * (s IDIII_hybrid_sb) + 2 <= 0 /\ 1 * (s IDIII_hybrid_bt) + -2 <= 0 /\ -1 * (s IDIII_hybrid_bt) + 2 <= 0 /\ -1 * (s IDIII_hybrid_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition III_hybrid_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((608 # 1)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 2%positive => ((608 # 1)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 3%positive => ((608 # 1)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 4%positive => ((608 # 1)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 5%positive => ((608 # 1)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 6%positive => ((608 # 1)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 7%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)
                                      - (s IDIII_hybrid_sb))
                     + (266 # 15) * max0((s IDIII_hybrid_sb)))%Q
    | 8%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)
                                      - (s IDIII_hybrid_sb))
                     + (266 # 15) * max0((s IDIII_hybrid_sb)))%Q
    | 9%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                     + (1 # 2) * max0(-1
                                      + (s IDIII_hybrid_gr_info_dref_off64))
                     + (1 # 2) * max0(1 + (s IDIII_hybrid_gr_info_dref_off64)
                                      - (s IDIII_hybrid_sb))
                     + (266 # 15) * max0((s IDIII_hybrid_sb)))%Q
    | 10%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64))
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (266 # 15) * max0((s IDIII_hybrid_sb)))%Q
    | 11%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64))
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (266 # 15) * max0((s IDIII_hybrid_sb)))%Q
    | 12%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64))
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (266 # 15) * max0((s IDIII_hybrid_sb)))%Q
    | 13%positive => ((570 # 1)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64)))%Q
    | 14%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 15%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 16%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 17%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 18%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 19%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + max0(-(s IDIII_hybrid_z)))%Q
    | 20%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + max0(-(s IDIII_hybrid_z)))%Q
    | 21%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 22%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 23%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 24%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 25%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(30 - (s IDIII_hybrid_sb)))%Q
    | 26%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(30 - (s IDIII_hybrid_sb)))%Q
    | 27%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 28%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 29%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 30%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 31%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 32%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 33%positive => ((608 # 1) - (19 # 1) * (s IDIII_hybrid_sb)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb)))%Q
    | 34%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 35%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 36%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 37%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 38%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 39%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 40%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 41%positive => ((s IDIII_hybrid_z))%Q
    | 42%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 43%positive => ((s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 44%positive => (-(18 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 45%positive => (-(18 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 46%positive => (-(18 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 47%positive => (-(18 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 48%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(31 - (s IDIII_hybrid_sb)))%Q
    | 49%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(31 - (s IDIII_hybrid_sb)))%Q
    | 50%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 51%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 52%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 53%positive => ((s IDIII_hybrid_z) + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 54%positive => (-(18 # 1) + (s IDIII_hybrid_z)
                      + max0(18 - (s IDIII_hybrid_i))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 55%positive => (-(s IDIII_hybrid_i) + (s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 56%positive => (-(s IDIII_hybrid_i) + (s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 57%positive => ((1 # 1) - (s IDIII_hybrid_i) + (s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 58%positive => ((1 # 1) - (s IDIII_hybrid_i) + (s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 59%positive => ((1 # 1) - (s IDIII_hybrid_i) + (s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 60%positive => (-(s IDIII_hybrid_i) + (s IDIII_hybrid_z)
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 61%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 62%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(30 - (s IDIII_hybrid_sb)))%Q
    | 63%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(-1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(30 - (s IDIII_hybrid_sb)))%Q
    | 64%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 65%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 66%positive => ((1 # 1) + (s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | 67%positive => ((s IDIII_hybrid_z)
                      + (1 # 2) * max0(1
                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                       - (s IDIII_hybrid_sb))
                      + (19 # 1) * max0(32 - (s IDIII_hybrid_sb)))%Q
    | _ => (0 # 1)%Q
  end.

Definition III_hybrid_hints (p : node) (s : state) := 
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
    | 11%positive => [(*0 17.7333*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDIII_hybrid_sb))) (F_check_ge (0) (0));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                  + (s IDIII_hybrid_gr_info_dref_off64))) (F_check_ge (0) (0))]
    | 12%positive => [(*0 1.26667*) F_max0_pre_decrement (32
                                                          - (s IDIII_hybrid_sb)) (2);
                      (*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDIII_hybrid_gr_info_dref_off64)
                                                               - (s IDIII_hybrid_sb)) (-1
                                                                    + (s IDIII_hybrid_gr_info_dref_off64)
                                                                    - (s IDIII_hybrid_sb)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDIII_hybrid_gr_info_dref_off64)
                                              - (s IDIII_hybrid_sb));
                      (*-1.26667 0*) F_max0_ge_0 (30 - (s IDIII_hybrid_sb));
                      (*-17.7333 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDIII_hybrid_sb))) (F_check_ge ((s IDIII_hybrid_sb)) (0));
                      (*0 1.26667*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                    - (s IDIII_hybrid_sb)) (0))) (F_max0_ge_0 (32
                                                                    - (s IDIII_hybrid_sb)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDIII_hybrid_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDIII_hybrid_z)))]
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDIII_hybrid_z))) (F_check_ge (0) (0));
                      (*-19 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                    - (s IDIII_hybrid_sb)) (0))) (F_max0_ge_0 (32
                                                                    - (s IDIII_hybrid_sb)))]
    | 21%positive => []
    | 22%positive => [(*0 0.5*) F_max0_monotonic (F_check_ge (1
                                                              + (s IDIII_hybrid_gr_info_dref_off64)
                                                              - (s IDIII_hybrid_sb)) (-1
                                                                    + (s IDIII_hybrid_gr_info_dref_off64)
                                                                    - (s IDIII_hybrid_sb)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   (s IDIII_hybrid_gr_info_dref_off64)
                                                                   - 
                                                                   (s IDIII_hybrid_sb))) (F_check_ge (0) (0))]
    | 23%positive => []
    | 24%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                                       - (s IDIII_hybrid_sb)) (2);
                      (*-19 0*) F_max0_monotonic (F_check_ge (32
                                                              - (s IDIII_hybrid_sb)) (30
                                                                    - (s IDIII_hybrid_sb)))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDIII_hybrid_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDIII_hybrid_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDIII_hybrid_z)));
                      (*-19 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                    - (s IDIII_hybrid_sb)) (0))) (F_max0_ge_0 (32
                                                                    - (s IDIII_hybrid_sb)))]
    | 34%positive => []
    | 35%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                                               + (s IDIII_hybrid_gr_info_dref_off64)
                                                               - (s IDIII_hybrid_sb)) (-1
                                                                    + (s IDIII_hybrid_gr_info_dref_off64)
                                                                    - (s IDIII_hybrid_sb)));
                      (*-0.5 0*) F_max0_ge_0 (-1
                                              + (s IDIII_hybrid_gr_info_dref_off64)
                                              - (s IDIII_hybrid_sb))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-19 0*) F_max0_monotonic (F_check_ge (32
                                                              - (s IDIII_hybrid_sb)) (30
                                                                    - (s IDIII_hybrid_sb)));
                      (*-19 0*) F_max0_ge_0 (30 - (s IDIII_hybrid_sb))]
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-19 0*) F_max0_pre_decrement (32
                                                      - (s IDIII_hybrid_sb)) (1)]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*0 1*) F_max0_monotonic (F_check_ge (18
                                                            - (s IDIII_hybrid_i)) (17
                                                                    - (s IDIII_hybrid_i)));
                      (*-1 0*) F_max0_ge_0 (17 - (s IDIII_hybrid_i))]
    | 54%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (18
                                                                  - (s IDIII_hybrid_i))) (F_check_ge (18
                                                                    - (s IDIII_hybrid_i)) (0))]
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18
                                                                    - (s IDIII_hybrid_i)) (0))) (F_max0_ge_0 (18
                                                                    - (s IDIII_hybrid_i)))]
    | 61%positive => [(*-0.5 0*) F_max0_pre_decrement (1
                                                       + (s IDIII_hybrid_gr_info_dref_off64)
                                                       - (s IDIII_hybrid_sb)) (2);
                      (*-19 0*) F_max0_monotonic (F_check_ge (32
                                                              - (s IDIII_hybrid_sb)) (30
                                                                    - (s IDIII_hybrid_sb)))]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | _ => []
  end.


Theorem III_hybrid_ai_correct:
  forall s p' s', steps (g_start III_hybrid) s (g_edges III_hybrid) p' s' -> III_hybrid_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem III_hybrid_pot_correct:
  forall s p' s',
    steps (g_start III_hybrid) s (g_edges III_hybrid) p' s' ->
    (III_hybrid_pot (g_start III_hybrid) s >= III_hybrid_pot p' s')%Q.
Proof.
  check_lp III_hybrid_ai_correct III_hybrid_hints.
Qed.

