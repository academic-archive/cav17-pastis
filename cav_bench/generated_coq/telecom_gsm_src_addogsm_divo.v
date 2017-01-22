Require Import pasta.Pasta.

Notation IDgsm_div_z := 1%positive.
Notation IDgsm_div_L_denum := 2%positive.
Notation IDgsm_div_L_num := 3%positive.
Notation IDgsm_div__tmp := 4%positive.
Notation IDgsm_div__tmp1 := 5%positive.
Notation IDgsm_div__tmp2 := 6%positive.
Notation IDgsm_div_div := 7%positive.
Notation IDgsm_div_k := 8%positive.
Notation IDgsm_div_denum := 9%positive.
Notation IDgsm_div_num := 10%positive.
Definition gsm_div : graph := {|
  g_start := 1%positive;
  g_end := 46%positive;
  g_edges := (1%positive,(AAssign IDgsm_div_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgsm_div__tmp
             (Some (EVar IDgsm_div_num))),3%positive)::
             (3%positive,(AAssign IDgsm_div__tmp1
             (Some (EVar IDgsm_div_denum))),4%positive)::
             (4%positive,(AAssign IDgsm_div_L_num
             (Some (EVar IDgsm_div__tmp))),5%positive)::
             (5%positive,(AAssign IDgsm_div_L_denum
             (Some (EVar IDgsm_div__tmp1))),6%positive)::
             (6%positive,(AAssign IDgsm_div_div (Some (ENum (0)))),
             7%positive)::
             (7%positive,(AAssign IDgsm_div_k (Some (ENum (15)))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDgsm_div__tmp) s) >=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDgsm_div__tmp) s) <
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,14%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDgsm_div__tmp1)
             s) >= (eval (EVar IDgsm_div__tmp) s))%Z)),16%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDgsm_div__tmp1)
             s) < (eval (EVar IDgsm_div__tmp) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,46%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDgsm_div__tmp) s) =
             (eval (ENum (0)) s))%Z)),42%positive)::
             (19%positive,(AGuard (fun s => ((eval (EVar IDgsm_div__tmp)
             s) <> (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDgsm_div_k (Some (EAdd (EVar IDgsm_div_k)
             (ENum (-1))))),23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDgsm_div_k) s) <>
             (eval (ENum (0)) s))%Z)),29%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDgsm_div_k) s) =
             (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDgsm_div__tmp2
             (Some (EVar IDgsm_div_div))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,46%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDgsm_div_div None),31%positive)::
             (31%positive,(AAssign IDgsm_div_L_num None),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDgsm_div_L_num)
             s) >= (eval (EVar IDgsm_div_L_denum) s))%Z)),35%positive)::
             (33%positive,(AGuard (fun s => ((eval (EVar IDgsm_div_L_num)
             s) < (eval (EVar IDgsm_div_L_denum) s))%Z)),34%positive)::
             (34%positive,AWeaken,39%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDgsm_div_L_num
             (Some (ESub (EVar IDgsm_div_L_num) (EVar IDgsm_div_L_denum)))),
             37%positive)::
             (37%positive,(AAssign IDgsm_div_div
             (Some (EAdd (EVar IDgsm_div_div) (ENum (1))))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDgsm_div_z (Some (EAdd (ENum (1))
             (EVar IDgsm_div_z)))),22%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AAssign IDgsm_div__tmp2 (Some (ENum (0)))),
             44%positive)::(44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::nil
|}.

Definition gsm_div_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0)%Z
    | 4%positive => (1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0)%Z
    | 6%positive => (1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 8%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0)%Z
    | 9%positive => (-1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 10%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div__tmp) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0)%Z
    | 12%positive => (-1 * (s IDgsm_div__tmp) <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 13%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ -1 * (s IDgsm_div__tmp)+ 1 * (s IDgsm_div__tmp1) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 15%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0)%Z
    | 16%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 17%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 18%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 19%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 20%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 22%positive => (-1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 23%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 24%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 25%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) <= 0 /\ -1 * (s IDgsm_div_k) <= 0)%Z
    | 26%positive => (-1 * (s IDgsm_div_k) <= 0 /\ 1 * (s IDgsm_div_k) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 27%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) <= 0 /\ -1 * (s IDgsm_div_k) <= 0)%Z
    | 28%positive => (-1 * (s IDgsm_div_k) <= 0 /\ 1 * (s IDgsm_div_k) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 29%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 30%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 31%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 32%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 33%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 34%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div_L_denum)+ 1 * (s IDgsm_div_L_num) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ 1 * (s IDgsm_div_L_denum)+ -1 * (s IDgsm_div_L_num) <= 0)%Z
    | 36%positive => (1 * (s IDgsm_div_L_denum)+ -1 * (s IDgsm_div_L_num) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 37%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div_L_num) <= 0)%Z
    | 38%positive => (-1 * (s IDgsm_div_L_num) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 39%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 40%positive => (1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -14 <= 0)%Z
    | 41%positive => (1 * (s IDgsm_div_k) + -14 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div__tmp) + 1 <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0)%Z
    | 42%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ 1 * (s IDgsm_div__tmp) <= 0)%Z
    | 43%positive => (1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 44%positive => (-1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ 1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp2) <= 0 /\ -1 * (s IDgsm_div__tmp2) <= 0)%Z
    | 45%positive => (-1 * (s IDgsm_div__tmp2) <= 0 /\ 1 * (s IDgsm_div__tmp2) <= 0 /\ 1 * (s IDgsm_div__tmp) <= 0 /\ 1 * (s IDgsm_div__tmp)+ -1 * (s IDgsm_div__tmp1) <= 0 /\ -1 * (s IDgsm_div__tmp) <= 0 /\ -1 * (s IDgsm_div_k) + 15 <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_div) <= 0 /\ -1 * (s IDgsm_div_div) <= 0)%Z
    | 46%positive => (-1 * (s IDgsm_div_k) <= 0 /\ -1 * (s IDgsm_div_z) <= 0 /\ 1 * (s IDgsm_div_k) + -15 <= 0)%Z
    | _ => False
  end.

Definition gsm_div_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((14 # 1))%Q
    | 2%positive => ((14 # 1) + (s IDgsm_div_z))%Q
    | 3%positive => ((14 # 1) + (s IDgsm_div_z))%Q
    | 4%positive => ((14 # 1) + (s IDgsm_div_z))%Q
    | 5%positive => ((14 # 1) + (s IDgsm_div_z))%Q
    | 6%positive => ((14 # 1) + (s IDgsm_div_z))%Q
    | 7%positive => ((14 # 1) + (s IDgsm_div_z))%Q
    | 8%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                     + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 9%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                     + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 10%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 11%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 12%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 13%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 14%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 15%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 16%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 17%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 18%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 19%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 20%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 21%positive => (-(1 # 1) + (s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 22%positive => (-(1 # 1) + (s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 23%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 24%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 25%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 26%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 27%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 28%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 29%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 30%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 31%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 32%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 33%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 34%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 35%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 36%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 37%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 38%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 39%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 40%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 41%positive => ((s IDgsm_div_k) + (s IDgsm_div_z))%Q
    | 42%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 43%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 44%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 45%positive => ((14 # 15) * (s IDgsm_div_k) + (s IDgsm_div_z)
                      + (1 # 15) * max0(-15 + (s IDgsm_div_k)))%Q
    | 46%positive => ((s IDgsm_div_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gsm_div_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-0.933333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgsm_div_k))) (F_check_ge (0) (0));
                      (*-0.933333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgsm_div_k)) (0))) (F_max0_ge_0 ((s IDgsm_div_k)));
                      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-15
                                                                    + (s IDgsm_div_k))) (F_check_ge (0) (0))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_arg (-15
                                                                    + (s IDgsm_div_k))) (F_check_ge (-15
                                                                    + (s IDgsm_div_k)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgsm_div_k))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgsm_div_k)) (0))) (F_max0_ge_0 ((s IDgsm_div_k)))]
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
    | 45%positive => [(*-0.933333 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDgsm_div_k))) (F_check_ge (0) (0));
                      (*-0.933333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgsm_div_k)) (0))) (F_max0_ge_0 ((s IDgsm_div_k)));
                      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-15
                                                                    + (s IDgsm_div_k))) (F_check_ge (0) (0))]
    | 46%positive => []
    | _ => []
  end.


Theorem gsm_div_ai_correct:
  forall s p' s', steps (g_start gsm_div) s (g_edges gsm_div) p' s' -> gsm_div_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gsm_div_pot_correct:
  forall s p' s',
    steps (g_start gsm_div) s (g_edges gsm_div) p' s' ->
    (gsm_div_pot (g_start gsm_div) s >= gsm_div_pot p' s')%Q.
Proof.
  check_lp gsm_div_ai_correct gsm_div_hints.
Qed.

