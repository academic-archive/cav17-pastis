Require Import pasta.Pasta.

Notation IDcompute_scalefacs_long_z := 1%positive.
Notation IDcompute_scalefacs_long_cod_info_dref_off64 := 2%positive.
Notation IDcompute_scalefacs_long_ifqstep_inv := 3%positive.
Notation IDcompute_scalefacs_long_sfb := 4%positive.
Notation IDcompute_scalefacs_long_cod_info := 5%positive.
Notation IDcompute_scalefacs_long_scalefac := 6%positive.
Notation IDcompute_scalefacs_long_vbrsf := 7%positive.
Definition compute_scalefacs_long : graph := {|
  g_start := 1%positive;
  g_end := 34%positive;
  g_edges := (1%positive,(AAssign IDcompute_scalefacs_long_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcompute_scalefacs_long_ifqstep_inv None),
             3%positive)::
             (3%positive,(AAssign
             IDcompute_scalefacs_long_cod_info_dref_off64 (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDcompute_scalefacs_long_sfb
             (Some (ENum (11)))),5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) <
             (eval (ENum (21)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) >=
             (eval (ENum (21)) s))%Z)),8%positive)::
             (8%positive,AWeaken,19%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,17%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDcompute_scalefacs_long_sfb
             (Some (EAdd (EVar IDcompute_scalefacs_long_sfb) (ENum (1))))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDcompute_scalefacs_long_z
             (Some (EAdd (ENum (1)) (EVar IDcompute_scalefacs_long_z)))),
             16%positive)::(16%positive,AWeaken,7%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) =
             (eval (ENum (21)) s))%Z)),21%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) <>
             (eval (ENum (21)) s))%Z)),20%positive)::
             (20%positive,AWeaken,29%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign
             IDcompute_scalefacs_long_cod_info_dref_off64 (Some (ENum (1)))),
             23%positive)::
             (23%positive,(AAssign IDcompute_scalefacs_long_sfb
             (Some (ENum (11)))),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) <
             (eval (ENum (21)) s))%Z)),51%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) >=
             (eval (ENum (21)) s))%Z)),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDcompute_scalefacs_long_sfb
             (Some (ENum (0)))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) <
             (eval (ENum (21)) s))%Z)),35%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) >=
             (eval (ENum (21)) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) <
             (eval (ENum (11)) s))%Z)),40%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDcompute_scalefacs_long_sfb) s) >=
             (eval (ENum (11)) s))%Z)),37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,43%positive)::
             (40%positive,AWeaken,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (43%positive,ANone,45%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDcompute_scalefacs_long_sfb
             (Some (EAdd (EVar IDcompute_scalefacs_long_sfb) (ENum (1))))),
             47%positive)::(47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDcompute_scalefacs_long_z
             (Some (EAdd (ENum (1)) (EVar IDcompute_scalefacs_long_z)))),
             50%positive)::(50%positive,AWeaken,32%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDcompute_scalefacs_long_sfb
             (Some (EAdd (EVar IDcompute_scalefacs_long_sfb) (ENum (1))))),
             54%positive)::(54%positive,ANone,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDcompute_scalefacs_long_z
             (Some (EAdd (ENum (1)) (EVar IDcompute_scalefacs_long_z)))),
             57%positive)::(57%positive,AWeaken,26%positive)::nil
|}.

Definition compute_scalefacs_long_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 4%positive => (1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 5%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0)%Z
    | 6%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 7%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 8%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0)%Z
    | 9%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 10%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 11%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 12%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 13%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0)%Z
    | 14%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 15%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0)%Z
    | 16%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 18%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 19%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 20%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 21%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0)%Z
    | 22%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 23%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0)%Z
    | 25%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 27%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0)%Z
    | 28%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 29%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 30%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0)%Z
    | 31%positive => (-1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 32%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 33%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0)%Z
    | 34%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 35%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 36%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 37%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0)%Z
    | 38%positive => (-1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 39%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0)%Z
    | 40%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -10 <= 0)%Z
    | 41%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -10 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 42%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -10 <= 0)%Z
    | 43%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 44%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 45%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0)%Z
    | 46%positive => (-1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 47%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 48%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 49%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 50%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 52%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0)%Z
    | 53%positive => (1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 11 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -20 <= 0)%Z
    | 54%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 55%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) <= 0)%Z
    | 56%positive => (-1 * (s IDcompute_scalefacs_long_z) <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0 /\ 1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0)%Z
    | 57%positive => (1 * (s IDcompute_scalefacs_long_sfb) + -21 <= 0 /\ -1 * (s IDcompute_scalefacs_long_sfb) + 12 <= 0 /\ 1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + -1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_cod_info_dref_off64) + 1 <= 0 /\ -1 * (s IDcompute_scalefacs_long_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition compute_scalefacs_long_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((41 # 1))%Q
    | 2%positive => ((41 # 1) + (s IDcompute_scalefacs_long_z))%Q
    | 3%positive => ((41 # 1) + (s IDcompute_scalefacs_long_z))%Q
    | 4%positive => ((20 # 1)
                     + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                     + (s IDcompute_scalefacs_long_z)
                     + (21 # 1) * max0(1
                                       - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 5%positive => ((31 # 1)
                     + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                     - (s IDcompute_scalefacs_long_sfb)
                     + (s IDcompute_scalefacs_long_z)
                     + (21 # 1) * max0(1
                                       - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 6%positive => ((31 # 1)
                     + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                     - (s IDcompute_scalefacs_long_sfb)
                     + (s IDcompute_scalefacs_long_z)
                     + (21 # 1) * max0(1
                                       - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 7%positive => ((31 # 1)
                     + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                     - (s IDcompute_scalefacs_long_sfb)
                     + (s IDcompute_scalefacs_long_z)
                     + (21 # 1) * max0(1
                                       - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 8%positive => ((31 # 1)
                     + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                     - (s IDcompute_scalefacs_long_sfb)
                     + (s IDcompute_scalefacs_long_z)
                     + (21 # 1) * max0(1
                                       - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 9%positive => ((31 # 1)
                     + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                     - (s IDcompute_scalefacs_long_sfb)
                     + (s IDcompute_scalefacs_long_z)
                     + (21 # 1) * max0(1
                                       - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 10%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 11%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 12%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 13%positive => ((32 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 14%positive => ((32 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 15%positive => ((32 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 16%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 17%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 18%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 19%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 20%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 21%positive => ((31 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 22%positive => ((31 # 1) + (s IDcompute_scalefacs_long_z))%Q
    | 23%positive => ((10 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 24%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 25%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 26%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 27%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 28%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 29%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 30%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 31%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      - (s IDcompute_scalefacs_long_sfb)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64)))%Q
    | 32%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 33%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 34%positive => ((s IDcompute_scalefacs_long_z))%Q
    | 35%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 36%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 37%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 38%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 39%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 40%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 41%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 42%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 43%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(20 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 44%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(20 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 45%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(20 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 46%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(20 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 47%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 48%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 49%positive => ((1 # 1) + (s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 50%positive => ((s IDcompute_scalefacs_long_z)
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 51%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 52%positive => ((1 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(20 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 53%positive => ((1 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(20 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 54%positive => ((1 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 55%positive => ((1 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 56%positive => ((1 # 1)
                      + (21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | 57%positive => ((21 # 1) * (s IDcompute_scalefacs_long_cod_info_dref_off64)
                      + (s IDcompute_scalefacs_long_z)
                      + (21 # 1) * max0(1
                                        - (s IDcompute_scalefacs_long_cod_info_dref_off64))
                      + max0(21 - (s IDcompute_scalefacs_long_sfb)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compute_scalefacs_long_hints (p : node) (s : state) := 
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
    | 20%positive => [(*0 11*) F_one;
                      (*-1 0*) F_max0_ge_0 (20
                                            - (s IDcompute_scalefacs_long_sfb));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDcompute_scalefacs_long_sfb)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDcompute_scalefacs_long_sfb)))]
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                                             - (s IDcompute_scalefacs_long_sfb)) (20
                                                                    - (s IDcompute_scalefacs_long_sfb)));
                      (*-1 0*) F_max0_ge_0 (20
                                            - (s IDcompute_scalefacs_long_sfb));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                                    - (s IDcompute_scalefacs_long_sfb)) (0))) (F_max0_ge_0 (21
                                                                    - (s IDcompute_scalefacs_long_sfb)));
                      (*-31 -10*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - (s IDcompute_scalefacs_long_cod_info_dref_off64))) (F_check_ge (1
                                                                    - (s IDcompute_scalefacs_long_cod_info_dref_off64)) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                                             - (s IDcompute_scalefacs_long_sfb)) (20
                                                                    - (s IDcompute_scalefacs_long_sfb)));
                      (*-1 0*) F_max0_ge_0 (20
                                            - (s IDcompute_scalefacs_long_sfb))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                                    - (s IDcompute_scalefacs_long_sfb)) (0))) (F_max0_ge_0 (21
                                                                    - (s IDcompute_scalefacs_long_sfb)));
                      (*-21 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    (s IDcompute_scalefacs_long_cod_info_dref_off64))) (F_check_ge (1
                                                                    - (s IDcompute_scalefacs_long_cod_info_dref_off64)) (0))]
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                                             - (s IDcompute_scalefacs_long_sfb)) (20
                                                                    - (s IDcompute_scalefacs_long_sfb)));
                      (*-1 0*) F_max0_ge_0 (20
                                            - (s IDcompute_scalefacs_long_sfb))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => [(*0 1*) F_max0_pre_decrement (21
                                                    - (s IDcompute_scalefacs_long_sfb)) (1)]
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_max0_pre_decrement (21
                                                     - (s IDcompute_scalefacs_long_sfb)) (1)]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcompute_scalefacs_long_z))) (F_check_ge ((s IDcompute_scalefacs_long_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcompute_scalefacs_long_z)) (0))) (F_max0_ge_0 ((s IDcompute_scalefacs_long_z)))]
    | 51%positive => [(*-1 0*) F_max0_pre_decrement (21
                                                     - (s IDcompute_scalefacs_long_sfb)) (1)]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | _ => []
  end.


Theorem compute_scalefacs_long_ai_correct:
  forall s p' s', steps (g_start compute_scalefacs_long) s (g_edges compute_scalefacs_long) p' s' -> compute_scalefacs_long_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compute_scalefacs_long_pot_correct:
  forall s p' s',
    steps (g_start compute_scalefacs_long) s (g_edges compute_scalefacs_long) p' s' ->
    (compute_scalefacs_long_pot (g_start compute_scalefacs_long) s >= compute_scalefacs_long_pot p' s')%Q.
Proof.
  check_lp compute_scalefacs_long_ai_correct compute_scalefacs_long_hints.
Qed.

