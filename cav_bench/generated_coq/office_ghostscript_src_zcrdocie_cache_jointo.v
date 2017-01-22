Require Import pasta.Pasta.

Notation IDcie_cache_joint_z := 1%positive.
Notation IDcie_cache_joint__tmp := 2%positive.
Notation IDcie_cache_joint_code := 3%positive.
Notation IDcie_cache_joint_es_code_ := 4%positive.
Notation IDcie_cache_joint_i := 5%positive.
Notation IDcie_cache_joint_j := 6%positive.
Notation IDcie_cache_joint_space := 7%positive.
Notation IDcie_cache_joint_pcrprocs := 8%positive.
Notation IDcie_cache_joint_pgs := 9%positive.
Definition cie_cache_joint : graph := {|
  g_start := 1%positive;
  g_end := 58%positive;
  g_edges := (1%positive,(AAssign IDcie_cache_joint_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,55%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,52%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDcie_cache_joint_code None),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_joint_code) s) <
             (eval (ENum (0)) s))%Z)),48%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_joint_code) s) >=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,16%positive)::
             (11%positive,(AAssign IDcie_cache_joint_es_code_ None),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_joint_es_code_) s) <
             (eval (ENum (0)) s))%Z)),44%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDcie_cache_joint_es_code_) s) >=
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDcie_cache_joint_space None),17%positive)::
             (17%positive,(AAssign IDcie_cache_joint_i (Some (ENum (0)))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDcie_cache_joint_i)
             s) < (eval (ENum (3)) s))%Z)),25%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDcie_cache_joint_i)
             s) >= (eval (ENum (3)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDcie_cache_joint__tmp None),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,58%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDcie_cache_joint_j (Some (ENum (0)))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDcie_cache_joint_j)
             s) < (eval (ENum (24)) s))%Z)),37%positive)::
             (29%positive,(AGuard (fun s => ((eval (EVar IDcie_cache_joint_j)
             s) >= (eval (ENum (24)) s))%Z)),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDcie_cache_joint_i
             (Some (EAdd (EVar IDcie_cache_joint_i) (ENum (1))))),
             33%positive)::(33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDcie_cache_joint_z (Some (EAdd (ENum (1))
             (EVar IDcie_cache_joint_z)))),36%positive)::
             (36%positive,AWeaken,20%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDcie_cache_joint_j
             (Some (EAdd (EVar IDcie_cache_joint_j) (ENum (1))))),
             40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDcie_cache_joint_z (Some (EAdd (ENum (1))
             (EVar IDcie_cache_joint_z)))),43%positive)::
             (43%positive,AWeaken,29%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AAssign IDcie_cache_joint__tmp
             (Some (EVar IDcie_cache_joint_es_code_))),46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,58%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AAssign IDcie_cache_joint__tmp
             (Some (EVar IDcie_cache_joint_code))),50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,AWeaken,58%positive)::
             (52%positive,(AAssign IDcie_cache_joint__tmp
             (Some (ENum (-25)))),53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,58%positive)::
             (55%positive,(AAssign IDcie_cache_joint__tmp (Some (ENum (0)))),
             56%positive)::(56%positive,ANone,57%positive)::
             (57%positive,AWeaken,58%positive)::nil
|}.

Definition cie_cache_joint_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 4%positive => (1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 6%positive => (1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 8%positive => (1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 14%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_es_code_) <= 0)%Z
    | 15%positive => (-1 * (s IDcie_cache_joint_es_code_) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 16%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 17%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 18%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0)%Z
    | 19%positive => (-1 * (s IDcie_cache_joint_i) <= 0 /\ 1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 20%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 21%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) + 3 <= 0)%Z
    | 22%positive => (-1 * (s IDcie_cache_joint_i) + 3 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 23%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) + 3 <= 0)%Z
    | 24%positive => (-1 * (s IDcie_cache_joint_i) + 3 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 25%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_i) + -2 <= 0)%Z
    | 26%positive => (1 * (s IDcie_cache_joint_i) + -2 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 27%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_i) + -2 <= 0 /\ 1 * (s IDcie_cache_joint_j) <= 0 /\ -1 * (s IDcie_cache_joint_j) <= 0)%Z
    | 28%positive => (-1 * (s IDcie_cache_joint_j) <= 0 /\ 1 * (s IDcie_cache_joint_j) <= 0 /\ 1 * (s IDcie_cache_joint_i) + -2 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 29%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_j) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0)%Z
    | 30%positive => (1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_j) + 24 <= 0)%Z
    | 31%positive => (-1 * (s IDcie_cache_joint_j) + 24 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0)%Z
    | 32%positive => (1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_j) + 24 <= 0)%Z
    | 33%positive => (-1 * (s IDcie_cache_joint_j) + 24 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDcie_cache_joint_i) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_j) + 24 <= 0)%Z
    | 35%positive => (-1 * (s IDcie_cache_joint_j) + 24 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDcie_cache_joint_i) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_j) + 24 <= 0 /\ -1 * (s IDcie_cache_joint_z) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_j) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_j) + -23 <= 0)%Z
    | 38%positive => (1 * (s IDcie_cache_joint_j) + -23 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_j) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 39%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_j) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_j) + -23 <= 0)%Z
    | 40%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_j) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0)%Z
    | 41%positive => (1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_j) + 1 <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 42%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_j) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_j) + -24 <= 0)%Z
    | 43%positive => (1 * (s IDcie_cache_joint_j) + -24 <= 0 /\ -1 * (s IDcie_cache_joint_j) + 1 <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0 /\ -1 * (s IDcie_cache_joint_i) <= 0 /\ -1 * (s IDcie_cache_joint_z) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_es_code_) + 1 <= 0)%Z
    | 45%positive => (1 * (s IDcie_cache_joint_es_code_) + 1 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 46%positive => (-1 * (s IDcie_cache_joint_code) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_es_code_) + 1 <= 0 /\ 1 * (s IDcie_cache_joint__tmp) + 1 <= 0)%Z
    | 47%positive => (1 * (s IDcie_cache_joint__tmp) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_es_code_) + 1 <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_code) <= 0)%Z
    | 48%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_code) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDcie_cache_joint_code) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 50%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_code) + 1 <= 0 /\ 1 * (s IDcie_cache_joint__tmp) + 1 <= 0)%Z
    | 51%positive => (1 * (s IDcie_cache_joint__tmp) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_code) + 1 <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 52%positive => (1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 53%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint__tmp) + 25 <= 0 /\ -1 * (s IDcie_cache_joint__tmp) + -25 <= 0)%Z
    | 54%positive => (-1 * (s IDcie_cache_joint__tmp) + -25 <= 0 /\ 1 * (s IDcie_cache_joint__tmp) + 25 <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 55%positive => (1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 56%positive => (-1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ 1 * (s IDcie_cache_joint__tmp) <= 0 /\ -1 * (s IDcie_cache_joint__tmp) <= 0)%Z
    | 57%positive => (-1 * (s IDcie_cache_joint__tmp) <= 0 /\ 1 * (s IDcie_cache_joint__tmp) <= 0 /\ 1 * (s IDcie_cache_joint_z) <= 0 /\ -1 * (s IDcie_cache_joint_z) <= 0)%Z
    | 58%positive => (-1 * (s IDcie_cache_joint_z) <= 0)%Z
    | _ => False
  end.

Definition cie_cache_joint_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((75 # 1))%Q
    | 2%positive => ((75 # 1) + max0((s IDcie_cache_joint_z)))%Q
    | 3%positive => ((75 # 1) + max0((s IDcie_cache_joint_z)))%Q
    | 4%positive => ((75 # 1) + max0((s IDcie_cache_joint_z)))%Q
    | 5%positive => ((75 # 1))%Q
    | 6%positive => ((75 # 1))%Q
    | 7%positive => ((75 # 1))%Q
    | 8%positive => ((75 # 1))%Q
    | 9%positive => ((75 # 1))%Q
    | 10%positive => ((75 # 1))%Q
    | 11%positive => ((75 # 1))%Q
    | 12%positive => ((75 # 1))%Q
    | 13%positive => ((75 # 1))%Q
    | 14%positive => ((75 # 1))%Q
    | 15%positive => ((75 # 1))%Q
    | 16%positive => ((75 # 1))%Q
    | 17%positive => ((75 # 1))%Q
    | 18%positive => ((25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 19%positive => ((25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 20%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 21%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 22%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 23%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 24%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 25%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 26%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i)))%Q
    | 27%positive => (-(24 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 28%positive => (-(24 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 29%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 30%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 31%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 32%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 33%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 34%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 35%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 36%positive => ((s IDcie_cache_joint_z)
                      + (25 # 1) * max0(3 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 37%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 38%positive => ((2 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(23 - (s IDcie_cache_joint_j)))%Q
    | 39%positive => ((2 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(23 - (s IDcie_cache_joint_j)))%Q
    | 40%positive => ((2 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 41%positive => ((2 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 42%positive => ((2 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 43%positive => ((1 # 1) + (s IDcie_cache_joint_z)
                      + (25 # 1) * max0(2 - (s IDcie_cache_joint_i))
                      + max0(24 - (s IDcie_cache_joint_j)))%Q
    | 44%positive => ((75 # 1))%Q
    | 45%positive => ((75 # 1) + (s IDcie_cache_joint_z)
                      + max0(-(s IDcie_cache_joint_z)))%Q
    | 46%positive => ((75 # 1) + (s IDcie_cache_joint_z)
                      + max0(-(s IDcie_cache_joint_z)))%Q
    | 47%positive => ((75 # 1) + (s IDcie_cache_joint_z)
                      + max0(-(s IDcie_cache_joint_z)))%Q
    | 48%positive => ((75 # 1))%Q
    | 49%positive => ((75 # 1) + (s IDcie_cache_joint_z)
                      + max0(-(s IDcie_cache_joint_z)))%Q
    | 50%positive => ((75 # 1) + (s IDcie_cache_joint_z)
                      + max0(-(s IDcie_cache_joint_z)))%Q
    | 51%positive => ((75 # 1) + (s IDcie_cache_joint_z)
                      + max0(-(s IDcie_cache_joint_z)))%Q
    | 52%positive => ((75 # 1))%Q
    | 53%positive => ((25 # 8) * max0(-1 - (s IDcie_cache_joint__tmp)))%Q
    | 54%positive => ((25 # 8) * max0(-1 - (s IDcie_cache_joint__tmp)))%Q
    | 55%positive => ((75 # 1) + max0((s IDcie_cache_joint_z)))%Q
    | 56%positive => ((3 # 1) * max0(25 + (s IDcie_cache_joint__tmp))
                      + max0((s IDcie_cache_joint_z)))%Q
    | 57%positive => ((3 # 1) * max0(25 + (s IDcie_cache_joint__tmp))
                      + max0((s IDcie_cache_joint_z)))%Q
    | 58%positive => ((s IDcie_cache_joint_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_cache_joint_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDcie_cache_joint_z))) (F_check_ge (0) (0))]
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
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcie_cache_joint_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcie_cache_joint_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcie_cache_joint_z)))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-25 0*) F_max0_monotonic (F_check_ge (3
                                                              - (s IDcie_cache_joint_i)) (2
                                                                    - (s IDcie_cache_joint_i)));
                      (*-25 0*) F_max0_ge_0 (2 - (s IDcie_cache_joint_i))]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-25 0*) F_max0_pre_decrement (3
                                                      - (s IDcie_cache_joint_i)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*0 1*) F_max0_monotonic (F_check_ge (24
                                                            - (s IDcie_cache_joint_j)) (23
                                                                    - (s IDcie_cache_joint_j)));
                      (*-1 0*) F_max0_ge_0 (23 - (s IDcie_cache_joint_j))]
    | 37%positive => [(*-1 0*) F_max0_pre_decrement (24
                                                     - (s IDcie_cache_joint_j)) (1)]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcie_cache_joint_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcie_cache_joint_z)))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-75 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcie_cache_joint_z))) (F_check_ge (0) (0))]
    | 48%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcie_cache_joint_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcie_cache_joint_z)))]
    | 49%positive => []
    | 50%positive => []
    | 51%positive => [(*-75 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcie_cache_joint_z))) (F_check_ge (0) (0))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDcie_cache_joint_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDcie_cache_joint_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDcie_cache_joint_z)));
                      (*-3.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - 
                                                                    (s IDcie_cache_joint__tmp))) (F_check_ge (0) (0))]
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcie_cache_joint_z))) (F_check_ge ((s IDcie_cache_joint_z)) (0));
                      (*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (25
                                                                 + (s IDcie_cache_joint__tmp))) (F_check_ge (0) (0))]
    | 58%positive => []
    | _ => []
  end.


Theorem cie_cache_joint_ai_correct:
  forall s p' s', steps (g_start cie_cache_joint) s (g_edges cie_cache_joint) p' s' -> cie_cache_joint_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_cache_joint_pot_correct:
  forall s p' s',
    steps (g_start cie_cache_joint) s (g_edges cie_cache_joint) p' s' ->
    (cie_cache_joint_pot (g_start cie_cache_joint) s >= cie_cache_joint_pot p' s')%Q.
Proof.
  check_lp cie_cache_joint_ai_correct cie_cache_joint_hints.
Qed.

