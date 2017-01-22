Require Import pasta.Pasta.

Notation IDideaCfbDecrypt_z := 1%positive.
Notation IDideaCfbDecrypt__tmp := 2%positive.
Notation IDideaCfbDecrypt_bufleft := 3%positive.
Notation IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref := 4%positive.
Notation IDideaCfbDecrypt_t := 5%positive.
Notation IDideaCfbDecrypt_context := 6%positive.
Notation IDideaCfbDecrypt_count := 7%positive.
Notation IDideaCfbDecrypt_dest := 8%positive.
Notation IDideaCfbDecrypt_src := 9%positive.
Definition ideaCfbDecrypt : graph := {|
  g_start := 1%positive;
  g_end := 60%positive;
  g_edges := (1%positive,(AAssign IDideaCfbDecrypt_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDideaCfbDecrypt__tmp
             (Some (EVar IDideaCfbDecrypt_count))),3%positive)::
             (3%positive,(AAssign IDideaCfbDecrypt_bufleft None),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt__tmp) s) <=
             (eval (EVar IDideaCfbDecrypt_bufleft) s))%Z)),52%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt__tmp) s) >
             (eval (EVar IDideaCfbDecrypt_bufleft) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDideaCfbDecrypt__tmp
             (Some (ESub (EVar IDideaCfbDecrypt__tmp)
             (EVar IDideaCfbDecrypt_bufleft)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDideaCfbDecrypt_bufleft
             (Some (EAdd (EVar IDideaCfbDecrypt_bufleft) (ENum (-1))))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt_bufleft) s) <>
             (eval (ENum (0)) s))%Z)),46%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt_bufleft) s) =
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt__tmp) s) >
             (eval (ENum (8)) s))%Z)),28%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt__tmp) s) <=
             (eval (ENum (8)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDideaCfbDecrypt_t
             (Some (EVar IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref))),
             19%positive)::
             (19%positive,(AAssign
             IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDideaCfbDecrypt__tmp
             (Some (EAdd (EVar IDideaCfbDecrypt__tmp) (ENum (-1))))),
             22%positive)::(22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbDecrypt__tmp) (ENum (-1)))
             s) <> (eval (ENum (0)) s))%Z)),25%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbDecrypt__tmp) (ENum (-1)))
             s) = (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,60%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDideaCfbDecrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbDecrypt_z)))),18%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDideaCfbDecrypt_bufleft
             (Some (ENum (8)))),30%positive)::
             (30%positive,(AAssign IDideaCfbDecrypt__tmp
             (Some (ESub (EVar IDideaCfbDecrypt__tmp) (ENum (8))))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDideaCfbDecrypt_t
             (Some (EVar IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref))),
             33%positive)::
             (33%positive,(AAssign
             IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref None),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDideaCfbDecrypt_bufleft
             (Some (EAdd (EVar IDideaCfbDecrypt_bufleft) (ENum (-1))))),
             36%positive)::(36%positive,AWeaken,37%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbDecrypt_bufleft)
             (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)),43%positive)::
             (37%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDideaCfbDecrypt_bufleft)
             (ENum (-1))) s) = (eval (ENum (0)) s))%Z)),38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDideaCfbDecrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbDecrypt_z)))),42%positive)::
             (42%positive,AWeaken,15%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,(AAssign IDideaCfbDecrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbDecrypt_z)))),32%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AAssign IDideaCfbDecrypt_t
             (Some (EVar IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref))),
             48%positive)::
             (48%positive,(AAssign
             IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref None),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDideaCfbDecrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbDecrypt_z)))),9%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDideaCfbDecrypt__tmp
             (Some (EAdd (EVar IDideaCfbDecrypt__tmp) (ENum (-1))))),
             55%positive)::(55%positive,AWeaken,56%positive)::
             (56%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt__tmp) s) <>
             (eval (ENum (0)) s))%Z)),61%positive)::
             (56%positive,(AGuard
             (fun s => ((eval (EVar IDideaCfbDecrypt__tmp) s) =
             (eval (ENum (0)) s))%Z)),57%positive)::
             (57%positive,AWeaken,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,AWeaken,60%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,(AAssign IDideaCfbDecrypt_t
             (Some (EVar IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref))),
             63%positive)::
             (63%positive,(AAssign
             IDideaCfbDecrypt_ideaCfbDecrypt.bufptr_dref None),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,(AAssign IDideaCfbDecrypt_z (Some (EAdd (ENum (1))
             (EVar IDideaCfbDecrypt_z)))),54%positive)::nil
|}.

Definition ideaCfbDecrypt_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 3%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 4%positive => (1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 5%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 6%positive => (1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp)+ 1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 7%positive => (-1 * (s IDideaCfbDecrypt__tmp)+ 1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 8%positive => (1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 11%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0)%Z
    | 13%positive => (-1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0)%Z
    | 15%positive => (-1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -8 <= 0)%Z
    | 17%positive => (1 * (s IDideaCfbDecrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0)%Z
    | 19%positive => (1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 20%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0)%Z
    | 21%positive => (1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 22%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDideaCfbDecrypt__tmp) + -7 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 24%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -7 <= 0)%Z
    | 26%positive => (1 * (s IDideaCfbDecrypt__tmp) + -7 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 27%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) + -7 <= 0)%Z
    | 28%positive => (1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 9 <= 0)%Z
    | 29%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 9 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0)%Z
    | 30%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 9 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) + 8 <= 0)%Z
    | 31%positive => (-1 * (s IDideaCfbDecrypt_bufleft) + 8 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 34%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -8 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 36%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -7 <= 0)%Z
    | 37%positive => (1 * (s IDideaCfbDecrypt_bufleft) + -7 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 38%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 40%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 42%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -7 <= 0)%Z
    | 44%positive => (1 * (s IDideaCfbDecrypt_bufleft) + -7 <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 45%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt_bufleft) + -7 <= 0)%Z
    | 46%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 47%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 49%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 51%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) + 1 <= 0)%Z
    | 52%positive => (1 * (s IDideaCfbDecrypt_z) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) <= 0)%Z
    | 53%positive => (1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 54%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) <= 0)%Z
    | 55%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 56%positive => (1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 57%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) <= 0)%Z
    | 58%positive => (-1 * (s IDideaCfbDecrypt__tmp) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 59%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) <= 0)%Z
    | 60%positive => (1 * (s IDideaCfbDecrypt__tmp) + -1 <= 0 /\ -1 * (s IDideaCfbDecrypt_bufleft) <= 0 /\ -1 * (s IDideaCfbDecrypt__tmp) <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 61%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 62%positive => (1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 63%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 64%positive => (1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | 65%positive => (-1 * (s IDideaCfbDecrypt_z) <= 0 /\ 1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0)%Z
    | 66%positive => (1 * (s IDideaCfbDecrypt__tmp)+ -1 * (s IDideaCfbDecrypt_bufleft) + 1 <= 0 /\ -1 * (s IDideaCfbDecrypt_z) <= 0)%Z
    | _ => False
  end.

Definition ideaCfbDecrypt_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDideaCfbDecrypt_count))%Q
    | 2%positive => ((s IDideaCfbDecrypt_count)
                     - max0(-(s IDideaCfbDecrypt_z)))%Q
    | 3%positive => ((s IDideaCfbDecrypt__tmp)
                     - max0(-(s IDideaCfbDecrypt_z)))%Q
    | 4%positive => ((s IDideaCfbDecrypt__tmp)
                     - max0(-(s IDideaCfbDecrypt_z)))%Q
    | 5%positive => ((s IDideaCfbDecrypt__tmp)
                     - max0(-(s IDideaCfbDecrypt_z)))%Q
    | 6%positive => ((s IDideaCfbDecrypt__tmp)
                     - max0(-(s IDideaCfbDecrypt_z)))%Q
    | 7%positive => ((s IDideaCfbDecrypt__tmp) + (s IDideaCfbDecrypt_z))%Q
    | 8%positive => ((s IDideaCfbDecrypt__tmp) + (s IDideaCfbDecrypt_bufleft)
                     + (s IDideaCfbDecrypt_z))%Q
    | 9%positive => ((s IDideaCfbDecrypt__tmp) + (s IDideaCfbDecrypt_bufleft)
                     + (s IDideaCfbDecrypt_z))%Q
    | 10%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 11%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 12%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 13%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 14%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 15%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 16%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 17%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 18%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 19%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 20%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 21%positive => ((1 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 22%positive => ((13 # 8) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(7 - (s IDideaCfbDecrypt__tmp)))%Q
    | 23%positive => (-(1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z))%Q
    | 24%positive => (-(1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z))%Q
    | 25%positive => (-(1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z))%Q
    | 26%positive => ((2 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 27%positive => ((2 # 1) + (5 # 8) * (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z)
                      - (3 # 8) * max0(8 - (s IDideaCfbDecrypt__tmp)))%Q
    | 28%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 29%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_z))%Q
    | 30%positive => (-(7 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 31%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 32%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 33%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 34%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 35%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 36%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 37%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 38%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 39%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 40%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 41%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 42%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 43%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 44%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 45%positive => ((2 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 46%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 47%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 48%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 49%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 50%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 51%positive => ((1 # 1) + (s IDideaCfbDecrypt__tmp)
                      + (s IDideaCfbDecrypt_bufleft) + (s IDideaCfbDecrypt_z))%Q
    | 52%positive => ((s IDideaCfbDecrypt__tmp)
                      - max0(-(s IDideaCfbDecrypt_z)))%Q
    | 53%positive => ((1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 54%positive => ((1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 55%positive => ((1 # 2) + (1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 56%positive => ((1 # 2) + (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 57%positive => ((1 # 2) + (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 58%positive => ((1 # 2) + (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 59%positive => ((1 # 2) + (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 60%positive => ((s IDideaCfbDecrypt_z))%Q
    | 61%positive => ((1 # 2) + (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-1 - (s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft))
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 62%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 63%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 64%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 65%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | 66%positive => ((1 # 1) + (1 # 2) * (s IDideaCfbDecrypt__tmp)
                      + (1 # 2) * (s IDideaCfbDecrypt_bufleft)
                      + (s IDideaCfbDecrypt_z)
                      - (1 # 2) * max0(-(s IDideaCfbDecrypt__tmp)
                                       + (s IDideaCfbDecrypt_bufleft)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ideaCfbDecrypt_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDideaCfbDecrypt_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDideaCfbDecrypt_z)))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbDecrypt_bufleft))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbDecrypt_bufleft)) (0))) (F_max0_ge_0 ((s IDideaCfbDecrypt_bufleft)));
                      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbDecrypt__tmp))) (F_check_ge (0) (0));
                      (*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbDecrypt__tmp)) (0))) (F_max0_ge_0 ((s IDideaCfbDecrypt__tmp)));
                      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    - 
                                                                    (s IDideaCfbDecrypt__tmp))) (F_check_ge (0) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - (s IDideaCfbDecrypt__tmp)) (0))) (F_max0_ge_0 (7
                                                                    - (s IDideaCfbDecrypt__tmp)))]
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-1
                                                             + (s IDideaCfbDecrypt__tmp)) (-9
                                                                    + (s IDideaCfbDecrypt__tmp)));
                      (*-1 0*) F_max0_ge_0 (-9 + (s IDideaCfbDecrypt__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDideaCfbDecrypt__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDideaCfbDecrypt__tmp)))]
    | 25%positive => [(*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                                    - (s IDideaCfbDecrypt__tmp))) (F_check_ge (8
                                                                    - (s IDideaCfbDecrypt__tmp)) (0))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbDecrypt_bufleft))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbDecrypt_bufleft)) (0))) (F_max0_ge_0 ((s IDideaCfbDecrypt_bufleft)))]
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
    | 51%positive => []
    | 52%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDideaCfbDecrypt_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDideaCfbDecrypt_z)));
                      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDideaCfbDecrypt__tmp)
                                                                    + 
                                                                    (s IDideaCfbDecrypt_bufleft))) (F_check_ge (-
                                                                    (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)) (0))]
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDideaCfbDecrypt__tmp)
                                                                    + 
                                                                    (s IDideaCfbDecrypt_bufleft))) (F_check_ge (-
                                                                    (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)) (0))]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => [(*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDideaCfbDecrypt__tmp))) (F_check_ge (0) (0));
                      (*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDideaCfbDecrypt__tmp)) (0))) (F_max0_ge_0 ((s IDideaCfbDecrypt__tmp)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)) (0))) (F_max0_ge_0 (-
                                                                    (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                    - 
                                                                    (s IDideaCfbDecrypt__tmp))) (F_check_ge (0) (0));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDideaCfbDecrypt__tmp)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDideaCfbDecrypt__tmp)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)))]
    | 60%positive => []
    | 61%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDideaCfbDecrypt__tmp)
                                                                    + (s IDideaCfbDecrypt_bufleft)))]
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | _ => []
  end.


Theorem ideaCfbDecrypt_ai_correct:
  forall s p' s', steps (g_start ideaCfbDecrypt) s (g_edges ideaCfbDecrypt) p' s' -> ideaCfbDecrypt_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ideaCfbDecrypt_pot_correct:
  forall s p' s',
    steps (g_start ideaCfbDecrypt) s (g_edges ideaCfbDecrypt) p' s' ->
    (ideaCfbDecrypt_pot (g_start ideaCfbDecrypt) s >= ideaCfbDecrypt_pot p' s')%Q.
Proof.
  check_lp ideaCfbDecrypt_ai_correct ideaCfbDecrypt_hints.
Qed.

