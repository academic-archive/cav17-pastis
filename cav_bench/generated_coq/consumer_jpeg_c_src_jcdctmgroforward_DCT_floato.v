Require Import pasta.Pasta.

Notation IDforward_DCT_float_z := 1%positive.
Notation IDforward_DCT_float__tmp := 2%positive.
Notation IDforward_DCT_float__tmp1 := 3%positive.
Notation IDforward_DCT_float__tmp2 := 4%positive.
Notation IDforward_DCT_float_bi := 5%positive.
Notation IDforward_DCT_float_elemr := 6%positive.
Notation IDforward_DCT_float_i := 7%positive.
Notation IDforward_DCT_float_cinfo := 8%positive.
Notation IDforward_DCT_float_coef_blocks := 9%positive.
Notation IDforward_DCT_float_compptr := 10%positive.
Notation IDforward_DCT_float_num_blocks := 11%positive.
Notation IDforward_DCT_float_sample_data := 12%positive.
Notation IDforward_DCT_float_start_col := 13%positive.
Notation IDforward_DCT_float_start_row := 14%positive.
Definition forward_DCT_float : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDforward_DCT_float_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_bi) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDforward_DCT_float__tmp2
             (Some (EVar IDforward_DCT_float_start_row))),6%positive)::
             (6%positive,(AAssign IDforward_DCT_float__tmp1
             (Some (EVar IDforward_DCT_float_start_col))),7%positive)::
             (7%positive,(AAssign IDforward_DCT_float__tmp
             (Some (EVar IDforward_DCT_float_num_blocks))),8%positive)::
             (8%positive,(AAssign IDforward_DCT_float_bi (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_bi) s) <
             (eval (EVar IDforward_DCT_float__tmp) s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_bi) s) >=
             (eval (EVar IDforward_DCT_float__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDforward_DCT_float_elemr
             (Some (ENum (0)))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_elemr) s) <
             (eval (ENum (8)) s))%Z)),39%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_elemr) s) >=
             (eval (ENum (8)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AAssign IDforward_DCT_float_i (Some (ENum (0)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_i) s) <
             (eval (ENum (64)) s))%Z)),32%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDforward_DCT_float_i) s) >=
             (eval (ENum (64)) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDforward_DCT_float_bi
             (Some (EAdd (EVar IDforward_DCT_float_bi) (ENum (1))))),
             27%positive)::
             (27%positive,(AAssign IDforward_DCT_float__tmp1
             (Some (EAdd (EVar IDforward_DCT_float__tmp1) (ENum (8))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDforward_DCT_float_z
             (Some (EAdd (ENum (1)) (EVar IDforward_DCT_float_z)))),
             31%positive)::(31%positive,AWeaken,11%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDforward_DCT_float_i
             (Some (EAdd (EVar IDforward_DCT_float_i) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDforward_DCT_float_z
             (Some (EAdd (ENum (1)) (EVar IDforward_DCT_float_z)))),
             38%positive)::(38%positive,AWeaken,23%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDforward_DCT_float_elemr
             (Some (EAdd (EVar IDforward_DCT_float_elemr) (ENum (1))))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDforward_DCT_float_z
             (Some (EAdd (ENum (1)) (EVar IDforward_DCT_float_z)))),
             45%positive)::(45%positive,AWeaken,18%positive)::nil
|}.

Definition forward_DCT_float_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0)%Z
    | 3%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 4%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDforward_DCT_float__tmp) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 6%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDforward_DCT_float__tmp) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 8%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0)%Z
    | 9%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 10%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0)%Z
    | 11%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 12%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float__tmp)+ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 13%positive => (1 * (s IDforward_DCT_float__tmp)+ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 14%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 16%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_elemr) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) <= 0)%Z
    | 17%positive => (-1 * (s IDforward_DCT_float_elemr) <= 0 /\ 1 * (s IDforward_DCT_float_elemr) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 18%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -8 <= 0)%Z
    | 19%positive => (1 * (s IDforward_DCT_float_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0)%Z
    | 20%positive => (-1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -8 <= 0)%Z
    | 21%positive => (1 * (s IDforward_DCT_float_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_float_i) <= 0 /\ -1 * (s IDforward_DCT_float_i) <= 0)%Z
    | 22%positive => (-1 * (s IDforward_DCT_float_i) <= 0 /\ 1 * (s IDforward_DCT_float_i) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -8 <= 0)%Z
    | 23%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_i) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0)%Z
    | 24%positive => (1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_i) + 64 <= 0)%Z
    | 25%positive => (-1 * (s IDforward_DCT_float_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0)%Z
    | 26%positive => (1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_i) + 64 <= 0)%Z
    | 27%positive => (-1 * (s IDforward_DCT_float_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 28%positive => (-1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_i) + 64 <= 0)%Z
    | 29%positive => (-1 * (s IDforward_DCT_float_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 30%positive => (-1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_i) + 64 <= 0)%Z
    | 31%positive => (-1 * (s IDforward_DCT_float_i) + 64 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_i) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_i) + -63 <= 0)%Z
    | 33%positive => (1 * (s IDforward_DCT_float_i) + -63 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_i) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0)%Z
    | 34%positive => (-1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_i) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_i) + -63 <= 0)%Z
    | 35%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_i) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0)%Z
    | 36%positive => (1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_i) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0)%Z
    | 37%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_i) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_i) + -64 <= 0)%Z
    | 38%positive => (1 * (s IDforward_DCT_float_i) + -64 <= 0 /\ -1 * (s IDforward_DCT_float_i) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 8 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -7 <= 0)%Z
    | 40%positive => (1 * (s IDforward_DCT_float_elemr) + -7 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0)%Z
    | 41%positive => (-1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -7 <= 0)%Z
    | 42%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -8 <= 0)%Z
    | 43%positive => (1 * (s IDforward_DCT_float_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) <= 0)%Z
    | 44%positive => (-1 * (s IDforward_DCT_float_z) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 1 <= 0 /\ 1 * (s IDforward_DCT_float_elemr) + -8 <= 0)%Z
    | 45%positive => (1 * (s IDforward_DCT_float_elemr) + -8 <= 0 /\ -1 * (s IDforward_DCT_float_elemr) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_bi) <= 0 /\ -1 * (s IDforward_DCT_float__tmp)+ 1 * (s IDforward_DCT_float_bi) + 1 <= 0 /\ -1 * (s IDforward_DCT_float_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition forward_DCT_float_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 2%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 3%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 4%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 5%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 6%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 7%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float_num_blocks)))%Q
    | 8%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float__tmp)))%Q
    | 9%positive => ((s IDforward_DCT_float_z)
                     + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                       - (s IDforward_DCT_float_bi)))%Q
    | 10%positive => ((s IDforward_DCT_float_z)
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 11%positive => ((s IDforward_DCT_float_z)
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 12%positive => ((s IDforward_DCT_float_z)
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 13%positive => ((s IDforward_DCT_float_z))%Q
    | 14%positive => ((s IDforward_DCT_float_z)
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 15%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 16%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 17%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 18%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 19%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 20%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 21%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      - (s IDforward_DCT_float_i) + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 22%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      - (s IDforward_DCT_float_i) + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 23%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 24%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 25%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 26%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 27%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 28%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 29%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 30%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 31%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + max0(-1 + (s IDforward_DCT_float_z))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 32%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 33%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 34%positive => ((65 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 35%positive => ((66 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 36%positive => ((66 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 37%positive => ((66 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(8 - (s IDforward_DCT_float_elemr))
                      + max0((s IDforward_DCT_float_z)))%Q
    | 38%positive => ((66 # 1) - (s IDforward_DCT_float_i)
                      + (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + max0(-1 + (s IDforward_DCT_float_z))
                      + max0(8 - (s IDforward_DCT_float_elemr)))%Q
    | 39%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 40%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 41%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 42%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 43%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 44%positive => (-(72 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | 45%positive => (-(73 # 1) + (73 # 1) * (s IDforward_DCT_float__tmp)
                      - (73 # 1) * (s IDforward_DCT_float_bi)
                      - (s IDforward_DCT_float_elemr)
                      + (s IDforward_DCT_float_z)
                      - (73 # 1) * max0(-1 + (s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi))
                      + (73 # 1) * max0((s IDforward_DCT_float__tmp)
                                        - (s IDforward_DCT_float_bi)))%Q
    | _ => (0 # 1)%Q
  end.

Definition forward_DCT_float_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-73 0*) F_max0_monotonic (F_check_ge ((s IDforward_DCT_float__tmp)
                                                              - (s IDforward_DCT_float_bi)) (-1
                                                                    + (s IDforward_DCT_float__tmp)
                                                                    - (s IDforward_DCT_float_bi)));
                      (*-73 0*) F_max0_ge_0 (-1
                                             + (s IDforward_DCT_float__tmp)
                                             - (s IDforward_DCT_float_bi))]
    | 13%positive => []
    | 14%positive => [(*0 73*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDforward_DCT_float__tmp)
                                                                   - 
                                                                   (s IDforward_DCT_float_bi))) (F_check_ge (-1
                                                                    + (s IDforward_DCT_float__tmp)
                                                                    - (s IDforward_DCT_float_bi)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-73 0*) F_max0_pre_decrement ((s IDforward_DCT_float__tmp)
                                                      - (s IDforward_DCT_float_bi)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDforward_DCT_float_z)) (0))) (F_max0_ge_0 ((s IDforward_DCT_float_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDforward_DCT_float_elemr)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDforward_DCT_float_elemr)));
                      (*-73 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDforward_DCT_float__tmp)
                                                                    - (s IDforward_DCT_float_bi)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDforward_DCT_float__tmp)
                                                                    - (s IDforward_DCT_float_bi)))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (64
                                                                 - (s IDforward_DCT_float_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDforward_DCT_float_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDforward_DCT_float_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                 - (s IDforward_DCT_float_elemr))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDforward_DCT_float_z))) (F_check_ge (-1
                                                                    + (s IDforward_DCT_float_z)) (0))]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDforward_DCT_float_z)) (0))) (F_max0_ge_0 ((s IDforward_DCT_float_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDforward_DCT_float_z))) (F_check_ge (-1
                                                                    + (s IDforward_DCT_float_z)) (0))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | _ => []
  end.


Theorem forward_DCT_float_ai_correct:
  forall s p' s', steps (g_start forward_DCT_float) s (g_edges forward_DCT_float) p' s' -> forward_DCT_float_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem forward_DCT_float_pot_correct:
  forall s p' s',
    steps (g_start forward_DCT_float) s (g_edges forward_DCT_float) p' s' ->
    (forward_DCT_float_pot (g_start forward_DCT_float) s >= forward_DCT_float_pot p' s')%Q.
Proof.
  check_lp forward_DCT_float_ai_correct forward_DCT_float_hints.
Qed.

