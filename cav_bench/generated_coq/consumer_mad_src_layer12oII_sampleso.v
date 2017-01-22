Require Import pasta.Pasta.

Notation IDII_samples_z := 1%positive.
Notation IDII_samples_c := 2%positive.
Notation IDII_samples_nb := 3%positive.
Notation IDII_samples_nlevels := 4%positive.
Notation IDII_samples_quantclass_dref_off0 := 5%positive.
Notation IDII_samples_quantclass_dref_off2 := 6%positive.
Notation IDII_samples_quantclass_dref_off3 := 7%positive.
Notation IDII_samples_requantized := 8%positive.
Notation IDII_samples_s := 9%positive.
Notation IDII_samples_output := 10%positive.
Notation IDII_samples_ptr := 11%positive.
Notation IDII_samples_quantclass := 12%positive.
Definition II_samples : graph := {|
  g_start := 1%positive;
  g_end := 36%positive;
  g_edges := (1%positive,(AAssign IDII_samples_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDII_samples_nb
             (Some (EVar IDII_samples_quantclass_dref_off2))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDII_samples_quantclass_dref_off2) s) <>
             (eval (ENum (0)) s))%Z)),22%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDII_samples_quantclass_dref_off2) s) =
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDII_samples_nb
             (Some (EVar IDII_samples_quantclass_dref_off3))),9%positive)::
             (9%positive,(AAssign IDII_samples_s (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s) s) <
             (eval (ENum (3)) s))%Z)),15%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s)
             s) >= (eval (ENum (3)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,31%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDII_samples_s
             (Some (EAdd (EVar IDII_samples_s) (ENum (1))))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDII_samples_z (Some (EAdd (ENum (1))
             (EVar IDII_samples_z)))),21%positive)::
             (21%positive,AWeaken,12%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AAssign IDII_samples_c None),24%positive)::
             (24%positive,(AAssign IDII_samples_nlevels
             (Some (EVar IDII_samples_quantclass_dref_off0))),25%positive)::
             (25%positive,(AAssign IDII_samples_s (Some (ENum (0)))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s) s) <
             (eval (ENum (3)) s))%Z)),47%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s)
             s) >= (eval (ENum (3)) s))%Z)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDII_samples_s (Some (ENum (0)))),
             32%positive)::(32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s) s) <
             (eval (ENum (3)) s))%Z)),37%positive)::
             (34%positive,(AGuard (fun s => ((eval (EVar IDII_samples_s)
             s) >= (eval (ENum (3)) s))%Z)),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDII_samples_requantized None),
             39%positive)::
             (39%positive,(AAssign IDII_samples_requantized None),
             40%positive)::
             (40%positive,(AAssign IDII_samples_requantized None),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDII_samples_s
             (Some (EAdd (EVar IDII_samples_s) (ENum (1))))),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDII_samples_z (Some (EAdd (ENum (1))
             (EVar IDII_samples_z)))),46%positive)::
             (46%positive,AWeaken,34%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,(AAssign IDII_samples_c None),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDII_samples_s
             (Some (EAdd (EVar IDII_samples_s) (ENum (1))))),51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDII_samples_z (Some (EAdd (ENum (1))
             (EVar IDII_samples_z)))),54%positive)::
             (54%positive,AWeaken,28%positive)::nil
|}.

Definition II_samples_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 3%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 4%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 5%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 6%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 7%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0)%Z
    | 8%positive => (-1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 9%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0)%Z
    | 10%positive => (-1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 11%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0)%Z
    | 12%positive => (-1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 13%positive => (1 * (s IDII_samples_s) + -3 <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) + 3 <= 0)%Z
    | 14%positive => (-1 * (s IDII_samples_s) + 3 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 16%positive => (1 * (s IDII_samples_s) + -2 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0)%Z
    | 17%positive => (1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 18%positive => (-1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 20%positive => (-1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_quantclass_dref_off2) <= 0 /\ -1 * (s IDII_samples_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 23%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 24%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 25%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 26%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 27%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 28%positive => (-1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 29%positive => (1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) + 3 <= 0)%Z
    | 30%positive => (-1 * (s IDII_samples_s) + 3 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 31%positive => (1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) + 3 <= 0)%Z
    | 32%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 33%positive => (-1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 34%positive => (-1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 35%positive => (1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) + 3 <= 0)%Z
    | 36%positive => (-1 * (s IDII_samples_s) + 3 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0)%Z
    | 37%positive => (-1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 38%positive => (1 * (s IDII_samples_s) + -2 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 39%positive => (-1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 40%positive => (1 * (s IDII_samples_s) + -2 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 41%positive => (-1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 42%positive => (1 * (s IDII_samples_s) + -2 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 43%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 45%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 48%positive => (1 * (s IDII_samples_s) + -2 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 49%positive => (-1 * (s IDII_samples_s) <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -2 <= 0)%Z
    | 50%positive => (1 * (s IDII_samples_s) + -2 <= 0 /\ -1 * (s IDII_samples_z) <= 0 /\ -1 * (s IDII_samples_s) <= 0)%Z
    | 51%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0)%Z
    | 52%positive => (-1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) <= 0)%Z
    | 53%positive => (-1 * (s IDII_samples_z) <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_s) + 1 <= 0)%Z
    | 54%positive => (-1 * (s IDII_samples_s) + 1 <= 0 /\ 1 * (s IDII_samples_s) + -3 <= 0 /\ -1 * (s IDII_samples_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition II_samples_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((6 # 1))%Q
    | 2%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 3%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 4%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 5%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 6%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 7%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 8%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 9%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 10%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 11%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 12%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 13%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 14%positive => ((3 # 1) + (s IDII_samples_z))%Q
    | 15%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 16%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 17%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 18%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 19%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 20%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 21%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 22%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 23%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 24%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 25%positive => ((6 # 1) + (s IDII_samples_z))%Q
    | 26%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 27%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 28%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 29%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 30%positive => ((3 # 1) + (s IDII_samples_z))%Q
    | 31%positive => ((3 # 1) + (s IDII_samples_z))%Q
    | 32%positive => ((s IDII_samples_z) + max0(3 - (s IDII_samples_s)))%Q
    | 33%positive => ((s IDII_samples_z) + max0(3 - (s IDII_samples_s)))%Q
    | 34%positive => ((s IDII_samples_z) + max0(3 - (s IDII_samples_s)))%Q
    | 35%positive => ((s IDII_samples_z) + max0(3 - (s IDII_samples_s)))%Q
    | 36%positive => ((s IDII_samples_z))%Q
    | 37%positive => ((s IDII_samples_z) + max0(3 - (s IDII_samples_s)))%Q
    | 38%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 39%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 40%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 41%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 42%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 43%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 44%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 45%positive => ((1 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 46%positive => ((s IDII_samples_z) + max0(3 - (s IDII_samples_s)))%Q
    | 47%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 48%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 49%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 50%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(2 - (s IDII_samples_s)))%Q
    | 51%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 52%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 53%positive => ((4 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | 54%positive => ((3 # 1) + (s IDII_samples_z)
                      + max0(3 - (s IDII_samples_s)))%Q
    | _ => (0 # 1)%Q
  end.

Definition II_samples_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDII_samples_s)) (2
                                                                    - (s IDII_samples_s)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDII_samples_s))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement (3 - (s IDII_samples_s)) (1)]
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
    | 29%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDII_samples_s)) (2
                                                                    - (s IDII_samples_s)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDII_samples_s))]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                             - (s IDII_samples_s)) (2
                                                                    - (s IDII_samples_s)));
                      (*-1 0*) F_max0_ge_0 (2 - (s IDII_samples_s))]
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_max0_pre_decrement (3 - (s IDII_samples_s)) (1)]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_max0_pre_decrement (3 - (s IDII_samples_s)) (1)]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | _ => []
  end.


Theorem II_samples_ai_correct:
  forall s p' s', steps (g_start II_samples) s (g_edges II_samples) p' s' -> II_samples_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem II_samples_pot_correct:
  forall s p' s',
    steps (g_start II_samples) s (g_edges II_samples) p' s' ->
    (II_samples_pot (g_start II_samples) s >= II_samples_pot p' s')%Q.
Proof.
  check_lp II_samples_ai_correct II_samples_hints.
Qed.

