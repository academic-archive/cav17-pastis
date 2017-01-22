Require Import pasta.Pasta.

Notation IDpbm_print_row_z := 1%positive.
Notation IDpbm_print_row__tmp := 2%positive.
Notation IDpbm_print_row_mask := 3%positive.
Notation IDpbm_print_row_pdev_dref_off1984 := 4%positive.
Notation IDpbm_print_row_pdev_dref_off64 := 5%positive.
Notation IDpbm_print_row_x := 6%positive.
Notation IDpbm_print_row_data := 7%positive.
Notation IDpbm_print_row_depth := 8%positive.
Notation IDpbm_print_row_pdev := 9%positive.
Notation IDpbm_print_row_pstream := 10%positive.
Definition pbm_print_row : graph := {|
  g_start := 1%positive;
  g_end := 37%positive;
  g_edges := (1%positive,(AAssign IDpbm_print_row_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpbm_print_row_x)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDpbm_print_row_pdev_dref_off64) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpbm_print_row__tmp
             (Some (EVar IDpbm_print_row_depth))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDpbm_print_row_pdev_dref_off1984) s) <>
             (eval (ENum (0)) s))%Z)),34%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDpbm_print_row_pdev_dref_off1984) s) =
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDpbm_print_row_x (Some (ENum (0)))),
             10%positive)::
             (10%positive,(AAssign IDpbm_print_row_mask (Some (ENum (128)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDpbm_print_row_x)
             s) < (eval (EVar IDpbm_print_row_pdev_dref_off64) s))%Z)),
             17%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDpbm_print_row_x)
             s) >= (eval (EVar IDpbm_print_row_pdev_dref_off64) s))%Z)),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,37%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDpbm_print_row_x
             (Some (EAdd (EVar IDpbm_print_row_x) (ENum (1))))),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDpbm_print_row_x) (ENum (1))) s) =
             (eval (EVar IDpbm_print_row_pdev_dref_off64) s))%Z)),
             23%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDpbm_print_row_x) (ENum (1)))
             s) <> (eval (EVar IDpbm_print_row_pdev_dref_off64) s))%Z)),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,ANone,25%positive)::
             (22%positive,ANone,24%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDpbm_print_row_mask None),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (27%positive,ANone,30%positive)::
             (28%positive,(AAssign IDpbm_print_row_mask (Some (ENum (128)))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDpbm_print_row_z (Some (EAdd (ENum (1))
             (EVar IDpbm_print_row_z)))),33%positive)::
             (33%positive,AWeaken,13%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,37%positive)::nil
|}.

Definition pbm_print_row_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 4%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0)%Z
    | 5%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 6%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0)%Z
    | 7%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 8%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 9%positive => (-1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 10%positive => (1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 11%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_mask) + -128 <= 0 /\ -1 * (s IDpbm_print_row_mask) + 128 <= 0)%Z
    | 12%positive => (-1 * (s IDpbm_print_row_mask) + 128 <= 0 /\ 1 * (s IDpbm_print_row_mask) + -128 <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 13%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 14%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off64)+ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 15%positive => (1 * (s IDpbm_print_row_pdev_dref_off64)+ -1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 16%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off64)+ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 17%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 19%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 21%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0)%Z
    | 22%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 23%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) + 1 <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off64)+ -1 * (s IDpbm_print_row_x) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 25%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0)%Z
    | 26%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 27%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0)%Z
    | 28%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 29%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_mask) + -128 <= 0 /\ -1 * (s IDpbm_print_row_mask) + 128 <= 0)%Z
    | 30%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 31%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0)%Z
    | 32%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ 1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0)%Z
    | 33%positive => (1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off1984) <= 0 /\ -1 * (s IDpbm_print_row_x) + 1 <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64)+ 1 * (s IDpbm_print_row_x) <= 0 /\ -1 * (s IDpbm_print_row_z) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0)%Z
    | 35%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | 36%positive => (-1 * (s IDpbm_print_row_x) <= 0 /\ 1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_pdev_dref_off64) <= 0)%Z
    | 37%positive => (-1 * (s IDpbm_print_row_pdev_dref_off64) <= 0 /\ -1 * (s IDpbm_print_row_z) <= 0 /\ -1 * (s IDpbm_print_row_x) <= 0)%Z
    | _ => False
  end.

Definition pbm_print_row_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 2%positive => ((s IDpbm_print_row_z)
                     + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 3%positive => ((s IDpbm_print_row_z)
                     + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 4%positive => ((s IDpbm_print_row_z)
                     + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 5%positive => (-(s IDpbm_print_row_pdev_dref_off64)
                     + (s IDpbm_print_row_z)
                     + (2 # 1) * max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 6%positive => (-(s IDpbm_print_row_pdev_dref_off64)
                     + (s IDpbm_print_row_z)
                     + (2 # 1) * max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 7%positive => ((s IDpbm_print_row_z)
                     + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 8%positive => ((s IDpbm_print_row_z)
                     + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 9%positive => ((s IDpbm_print_row_z)
                     + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 10%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 11%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 12%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 13%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 14%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 15%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 16%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 17%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 18%positive => ((s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 19%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 20%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 21%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 22%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 23%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 24%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 25%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 26%positive => ((1 # 1) + (s IDpbm_print_row_pdev_dref_off64)
                      - (s IDpbm_print_row_x) + (s IDpbm_print_row_z))%Q
    | 27%positive => ((1 # 1) + (s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 28%positive => ((1 # 1) + (s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 29%positive => ((1 # 1) + (s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 30%positive => ((1 # 1) + (s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 31%positive => ((1 # 1) + (s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 32%positive => ((1 # 1) + (s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 33%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)
                             - (s IDpbm_print_row_x)))%Q
    | 34%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 35%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 36%positive => ((s IDpbm_print_row_z)
                      + max0((s IDpbm_print_row_pdev_dref_off64)))%Q
    | 37%positive => ((s IDpbm_print_row_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pbm_print_row_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpbm_print_row_pdev_dref_off64)) (0))) (F_max0_ge_0 ((s IDpbm_print_row_pdev_dref_off64)))]
    | 5%positive => []
    | 6%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpbm_print_row_pdev_dref_off64))) (F_check_ge ((s IDpbm_print_row_pdev_dref_off64)) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpbm_print_row_pdev_dref_off64)
                                                             - (s IDpbm_print_row_x)) (-1
                                                                    + (s IDpbm_print_row_pdev_dref_off64)
                                                                    - (s IDpbm_print_row_x)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDpbm_print_row_pdev_dref_off64)
                                            - (s IDpbm_print_row_x))]
    | 17%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDpbm_print_row_pdev_dref_off64)
                                                                   - 
                                                                   (s IDpbm_print_row_x))) (F_check_ge ((s IDpbm_print_row_pdev_dref_off64)
                                                                    - (s IDpbm_print_row_x)) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpbm_print_row_pdev_dref_off64)
                                                                    - (s IDpbm_print_row_x)) (0))) (F_max0_ge_0 ((s IDpbm_print_row_pdev_dref_off64)
                                                                    - (s IDpbm_print_row_x)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_max0_ge_0 ((s IDpbm_print_row_pdev_dref_off64))]
    | 37%positive => []
    | _ => []
  end.


Theorem pbm_print_row_ai_correct:
  forall s p' s', steps (g_start pbm_print_row) s (g_edges pbm_print_row) p' s' -> pbm_print_row_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pbm_print_row_pot_correct:
  forall s p' s',
    steps (g_start pbm_print_row) s (g_edges pbm_print_row) p' s' ->
    (pbm_print_row_pot (g_start pbm_print_row) s >= pbm_print_row_pot p' s')%Q.
Proof.
  check_lp pbm_print_row_ai_correct pbm_print_row_hints.
Qed.

