Require Import pasta.Pasta.

Notation IDLARp_to_rp_z := 1%positive.
Notation IDLARp_to_rp_i := 2%positive.
Notation IDLARp_to_rp_ltmp := 3%positive.
Notation IDLARp_to_rp_temp := 4%positive.
Notation IDLARp_to_rp_LARp := 5%positive.
Definition LARp_to_rp : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDLARp_to_rp_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDLARp_to_rp_i (Some (ENum (1)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDLARp_to_rp_i) s) <=
             (eval (ENum (8)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDLARp_to_rp_i) s) >
             (eval (ENum (8)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,25%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDLARp_to_rp_temp None),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,23%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,21%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDLARp_to_rp_ltmp None),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,19%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,20%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,22%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,24%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,45%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,42%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDLARp_to_rp_temp None),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard (fun s => True)),41%positive)::
             (30%positive,(AGuard (fun s => True)),31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,39%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDLARp_to_rp_ltmp None),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,37%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,38%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,40%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,44%positive)::
             (41%positive,AWeaken,43%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDLARp_to_rp_i
             (Some (EAdd (EVar IDLARp_to_rp_i) (ENum (1))))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDLARp_to_rp_z (Some (EAdd (ENum (1))
             (EVar IDLARp_to_rp_z)))),50%positive)::
             (50%positive,AWeaken,5%positive)::nil
|}.

Definition LARp_to_rp_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0)%Z
    | 3%positive => (-1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -1 <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ 1 * (s IDLARp_to_rp_i) + -1 <= 0 /\ 1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0)%Z
    | 5%positive => (-1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ 1 * (s IDLARp_to_rp_i) + -9 <= 0)%Z
    | 6%positive => (1 * (s IDLARp_to_rp_i) + -9 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 9 <= 0)%Z
    | 7%positive => (-1 * (s IDLARp_to_rp_i) + 9 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -9 <= 0)%Z
    | 8%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 9%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 11%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 13%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 15%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 17%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 19%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 20%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 23%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 25%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 26%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 28%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 30%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 32%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 34%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 36%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 37%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 39%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 40%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 42%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 43%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 45%positive => (1 * (s IDLARp_to_rp_i) + -8 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 1 <= 0)%Z
    | 46%positive => (-1 * (s IDLARp_to_rp_i) + 1 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0 /\ 1 * (s IDLARp_to_rp_i) + -8 <= 0)%Z
    | 47%positive => (-1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 2 <= 0 /\ 1 * (s IDLARp_to_rp_i) + -9 <= 0)%Z
    | 48%positive => (1 * (s IDLARp_to_rp_i) + -9 <= 0 /\ -1 * (s IDLARp_to_rp_i) + 2 <= 0 /\ -1 * (s IDLARp_to_rp_z) <= 0)%Z
    | 49%positive => (-1 * (s IDLARp_to_rp_z) <= 0 /\ -1 * (s IDLARp_to_rp_i) + 2 <= 0 /\ 1 * (s IDLARp_to_rp_i) + -9 <= 0)%Z
    | 50%positive => (1 * (s IDLARp_to_rp_i) + -9 <= 0 /\ -1 * (s IDLARp_to_rp_i) + 2 <= 0 /\ -1 * (s IDLARp_to_rp_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition LARp_to_rp_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDLARp_to_rp_z))%Q
    | 3%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 4%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 5%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 6%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 7%positive => ((s IDLARp_to_rp_z))%Q
    | 8%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 9%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 10%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 11%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 12%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 13%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 14%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 15%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 16%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 17%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 18%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 19%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 20%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 21%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 22%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 23%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 24%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 25%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 26%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 27%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 28%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 29%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 30%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 31%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 32%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 33%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 34%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 35%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 36%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 37%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 38%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 39%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 40%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 41%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 42%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 43%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 44%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 45%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 46%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(8 - (s IDLARp_to_rp_i)))%Q
    | 47%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 48%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 49%positive => ((1 # 1) + (s IDLARp_to_rp_z)
                      + max0(9 - (s IDLARp_to_rp_i)))%Q
    | 50%positive => ((s IDLARp_to_rp_z) + max0(9 - (s IDLARp_to_rp_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition LARp_to_rp_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (9
                                                            - (s IDLARp_to_rp_i)) (8
                                                                    - (s IDLARp_to_rp_i)));
                     (*-1 0*) F_max0_ge_0 (8 - (s IDLARp_to_rp_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement (9 - (s IDLARp_to_rp_i)) (1)]
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
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (9 - (s IDLARp_to_rp_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
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
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | _ => []
  end.


Theorem LARp_to_rp_ai_correct:
  forall s p' s', steps (g_start LARp_to_rp) s (g_edges LARp_to_rp) p' s' -> LARp_to_rp_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem LARp_to_rp_pot_correct:
  forall s p' s',
    steps (g_start LARp_to_rp) s (g_edges LARp_to_rp) p' s' ->
    (LARp_to_rp_pot (g_start LARp_to_rp) s >= LARp_to_rp_pot p' s')%Q.
Proof.
  check_lp LARp_to_rp_ai_correct LARp_to_rp_hints.
Qed.

