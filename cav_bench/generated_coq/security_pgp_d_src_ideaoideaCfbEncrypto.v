Require Import pasta.Pasta.

Notation IDideaCfbEncrypt_z := 1%positive.
Notation IDideaCfbEncrypt__tmp := 2%positive.
Notation IDideaCfbEncrypt_bufleft := 3%positive.
Notation IDideaCfbEncrypt_context := 4%positive.
Notation IDideaCfbEncrypt_count := 5%positive.
Notation IDideaCfbEncrypt_dest := 6%positive.
Notation IDideaCfbEncrypt_src := 7%positive.
Definition ideaCfbEncrypt : graph := {|
  g_start := 1%positive;
  g_end := 54%positive;
  g_edges := (1%positive,(AAssign IDideaCfbEncrypt_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDideaCfbEncrypt__tmp
             (Some (EVar IDideaCfbEncrypt_count))),3%positive)::
             (3%positive,(AAssign IDideaCfbEncrypt_bufleft None),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt__tmp) s) <=
             (eval (EVar IDideaCfbEncrypt_bufleft) s))%Z)),46%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt__tmp) s) >
             (eval (EVar IDideaCfbEncrypt_bufleft) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDideaCfbEncrypt__tmp
             (Some (ESub (EVar IDideaCfbEncrypt__tmp)
             (EVar IDideaCfbEncrypt_bufleft)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDideaCfbEncrypt_bufleft
             (Some (EAdd (EVar IDideaCfbEncrypt_bufleft) (ENum (-1))))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt_bufleft) s) <>
             (eval (ENum (0)) s))%Z)),42%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt_bufleft) s) =
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt__tmp) s) >
             (eval (ENum (8)) s))%Z)),26%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt__tmp) s) <=
             (eval (ENum (8)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDideaCfbEncrypt__tmp
             (Some (EAdd (EVar IDideaCfbEncrypt__tmp) (ENum (-1))))),
             20%positive)::(20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbEncrypt__tmp) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),23%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbEncrypt__tmp) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),22%positive)::
             (22%positive,AWeaken,54%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDideaCfbEncrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbEncrypt_z)))),18%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDideaCfbEncrypt_bufleft
             (Some (ENum (8)))),28%positive)::
             (28%positive,(AAssign IDideaCfbEncrypt__tmp
             (Some (ESub (EVar IDideaCfbEncrypt__tmp) (ENum (8))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDideaCfbEncrypt_bufleft
             (Some (EAdd (EVar IDideaCfbEncrypt_bufleft) (ENum (-1))))),
             32%positive)::(32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbEncrypt_bufleft)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),39%positive)::
             (33%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbEncrypt_bufleft)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),34%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDideaCfbEncrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbEncrypt_z)))),38%positive)::
             (38%positive,AWeaken,15%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDideaCfbEncrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbEncrypt_z)))),30%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDideaCfbEncrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbEncrypt_z)))),9%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,(AAssign IDideaCfbEncrypt__tmp
             (Some (EAdd (EVar IDideaCfbEncrypt__tmp) (ENum (-1))))),
             49%positive)::(49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt__tmp) s) <>
             (eval (ENum (0)) s))%Z)),55%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbEncrypt__tmp) s) =
             (eval (ENum (0)) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,AWeaken,54%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,(AAssign IDideaCfbEncrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbEncrypt_z)))),48%positive)::nil
|}.

Definition ideaCfbEncrypt_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 3%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 4%positive => (1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 5%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 6%positive => (1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp)+ 1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0)%Z
    | 7%positive => (-1 * (s IDideaCfbEncrypt__tmp)+ 1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 8%positive => (1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 11%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0)%Z
    | 13%positive => (-1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0)%Z
    | 15%positive => (-1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -8 <= 0)%Z
    | 17%positive => (1 * (s IDideaCfbEncrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0)%Z
    | 19%positive => (1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 20%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -7 <= 0)%Z
    | 21%positive => (1 * (s IDideaCfbEncrypt__tmp) + -7 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 22%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -7 <= 0)%Z
    | 24%positive => (1 * (s IDideaCfbEncrypt__tmp) + -7 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 25%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) + -7 <= 0)%Z
    | 26%positive => (1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 9 <= 0)%Z
    | 27%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 9 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0)%Z
    | 28%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 9 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) + 8 <= 0)%Z
    | 29%positive => (-1 * (s IDideaCfbEncrypt_bufleft) + 8 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 32%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -7 <= 0)%Z
    | 33%positive => (1 * (s IDideaCfbEncrypt_bufleft) + -7 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 34%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 36%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 38%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -7 <= 0)%Z
    | 40%positive => (1 * (s IDideaCfbEncrypt_bufleft) + -7 <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 41%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt_bufleft) + -7 <= 0)%Z
    | 42%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 43%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 45%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) + 1 <= 0)%Z
    | 46%positive => (1 * (s IDideaCfbEncrypt_z) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) <= 0)%Z
    | 47%positive => (1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 48%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) <= 0)%Z
    | 49%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0)%Z
    | 50%positive => (1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 51%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) <= 0)%Z
    | 52%positive => (-1 * (s IDideaCfbEncrypt__tmp) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 53%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) <= 0)%Z
    | 54%positive => (1 * (s IDideaCfbEncrypt__tmp) + -1 <= 0 /\ -1 * (s IDideaCfbEncrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbEncrypt__tmp) <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 55%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0)%Z
    | 56%positive => (1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | 57%positive => (-1 * (s IDideaCfbEncrypt_z) <= 0 /\ 1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0)%Z
    | 58%positive => (1 * (s IDideaCfbEncrypt__tmp)+ -1 * (s IDideaCfbEncrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbEncrypt_z) <= 0)%Z
    | _ => False
  end.

Definition ideaCfbEncrypt_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDideaCfbEncrypt_count))%Q
    | 2%positive => ((s IDideaCfbEncrypt_count)
                     - max0(-(s IDideaCfbEncrypt_z)))%Q
    | 3%positive => ((s IDideaCfbEncrypt__tmp)
                     - max0(-(s IDideaCfbEncrypt_z)))%Q
    | 4%positive => ((s IDideaCfbEncrypt__tmp)
                     - max0(-(s IDideaCfbEncrypt_z)))%Q
    | 5%positive => ((s IDideaCfbEncrypt__tmp)
                     - max0(-(s IDideaCfbEncrypt_z)))%Q
    | 6%positive => ((s IDideaCfbEncrypt__tmp)
                     - max0(-(s IDideaCfbEncrypt_z)))%Q
    | 7%positive => ((s IDideaCfbEncrypt__tmp) + (s IDideaCfbEncrypt_z))%Q
    | 8%positive => ((s IDideaCfbEncrypt__tmp) + (s IDideaCfbEncrypt_bufleft)
                     + (s IDideaCfbEncrypt_z))%Q
    | 9%positive => ((s IDideaCfbEncrypt__tmp) + (s IDideaCfbEncrypt_bufleft)
                     + (s IDideaCfbEncrypt_z))%Q
    | 10%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 11%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 12%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 13%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 14%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 15%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 16%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 17%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbEncrypt__tmp)))%Q
    | 18%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbEncrypt__tmp)))%Q
    | 19%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbEncrypt__tmp)))%Q
    | 20%positive => ((13 # 8) + (5 # 8) * (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z)
                      - (3 # 8) * max0(7 - (s IDideaCfbEncrypt__tmp)))%Q
    | 21%positive => (-(1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z))%Q
    | 22%positive => (-(1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z))%Q
    | 23%positive => (-(1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z))%Q
    | 24%positive => ((2 # 1) + (5 # 8) * (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbEncrypt__tmp)))%Q
    | 25%positive => ((2 # 1) + (5 # 8) * (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbEncrypt__tmp)))%Q
    | 26%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 27%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_z))%Q
    | 28%positive => (-(7 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 29%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 30%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 31%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 32%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 33%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 34%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 35%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 36%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 37%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 38%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 39%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 40%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 41%positive => ((2 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 42%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 43%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 44%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 45%positive => ((1 # 1) + (s IDideaCfbEncrypt__tmp)
                      + (s IDideaCfbEncrypt_bufleft) + (s IDideaCfbEncrypt_z))%Q
    | 46%positive => ((s IDideaCfbEncrypt__tmp)
                      - max0(-(s IDideaCfbEncrypt_z)))%Q
    | 47%positive => ((1 # 2) * (s IDideaCfbEncrypt__tmp)
                      + (1 # 2) * (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 48%positive => ((1 # 2) * (s IDideaCfbEncrypt__tmp)
                      + (1 # 2) * (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 49%positive => ((1 # 2) + (1 # 2) * (s IDideaCfbEncrypt__tmp)
                      + (1 # 2) * (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 50%positive => ((1 # 2) + (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 51%positive => ((1 # 2) + (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 52%positive => ((1 # 2) + (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 53%positive => ((1 # 2) + (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 54%positive => ((s IDideaCfbEncrypt_z))%Q
    | 55%positive => ((1 # 2) + (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 56%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbEncrypt__tmp)
                      + (1 # 2) * (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 57%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbEncrypt__tmp)
                      + (1 # 2) * (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | 58%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbEncrypt__tmp)
                      + (1 # 2) * (s IDideaCfbEncrypt_bufleft)
                      + (s IDideaCfbEncrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbEncrypt__tmp)
                                       + (s IDideaCfbEncrypt_bufleft)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ideaCfbEncrypt_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDideaCfbEncrypt_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDideaCfbEncrypt_z)))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbEncrypt_bufleft))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbEncrypt_bufleft)) (0))) (F_max0_ge_0 ((s IDideaCfbEncrypt_bufleft)));
                      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbEncrypt__tmp))) (F_check_ge (0) (0));
                      (*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbEncrypt__tmp)) (0))) (F_max0_ge_0 ((s IDideaCfbEncrypt__tmp)));
                      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    - 
                                                                    (s IDideaCfbEncrypt__tmp))) (F_check_ge (0) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDideaCfbEncrypt__tmp)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDideaCfbEncrypt__tmp)))]
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-1
                                                             + (s IDideaCfbEncrypt__tmp)) (-9
                                                                    + (s IDideaCfbEncrypt__tmp)));
                      (*-1 0*) F_max0_ge_0 (-9 + (s IDideaCfbEncrypt__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDideaCfbEncrypt__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDideaCfbEncrypt__tmp)))]
    | 23%positive => [(*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDideaCfbEncrypt__tmp))) (F_check_ge (8
                                                                    - (s IDideaCfbEncrypt__tmp)) (0))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbEncrypt_bufleft))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbEncrypt_bufleft)) (0))) (F_max0_ge_0 ((s IDideaCfbEncrypt_bufleft)))]
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
    | 46%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDideaCfbEncrypt_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDideaCfbEncrypt_z)));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDideaCfbEncrypt__tmp)
                                                                    + 
                                                                    (s IDideaCfbEncrypt_bufleft))) (F_check_ge (-
                                                                    (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)) (0))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDideaCfbEncrypt__tmp)
                                                                    + 
                                                                    (s IDideaCfbEncrypt_bufleft))) (F_check_ge (-
                                                                    (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)) (0))]
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => [(*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbEncrypt__tmp))) (F_check_ge (0) (0));
                      (*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbEncrypt__tmp)) (0))) (F_max0_ge_0 ((s IDideaCfbEncrypt__tmp)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)) (0))) (F_max0_ge_0 (-
                                                                    (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    - 
                                                                    (s IDideaCfbEncrypt__tmp))) (F_check_ge (0) (0));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDideaCfbEncrypt__tmp)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDideaCfbEncrypt__tmp)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)))]
    | 54%positive => []
    | 55%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDideaCfbEncrypt__tmp)
                                                                    + (s IDideaCfbEncrypt_bufleft)))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | _ => []
  end.


Theorem ideaCfbEncrypt_ai_correct:
  forall s p' s', steps (g_start ideaCfbEncrypt) s (g_edges ideaCfbEncrypt) p' s' -> ideaCfbEncrypt_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ideaCfbEncrypt_pot_correct:
  forall s p' s',
    steps (g_start ideaCfbEncrypt) s (g_edges ideaCfbEncrypt) p' s' ->
    (ideaCfbEncrypt_pot (g_start ideaCfbEncrypt) s >= ideaCfbEncrypt_pot p' s')%Q.
Proof.
  check_lp ideaCfbEncrypt_ai_correct ideaCfbEncrypt_hints.
Qed.

