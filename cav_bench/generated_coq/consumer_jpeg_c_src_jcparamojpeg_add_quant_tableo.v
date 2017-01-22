Require Import pasta.Pasta.

Notation IDjpeg_add_quant_table_z := 1%positive.
Notation IDjpeg_add_quant_table__tmp := 2%positive.
Notation IDjpeg_add_quant_table__tmp1 := 3%positive.
Notation IDjpeg_add_quant_table__tmp2 := 4%positive.
Notation IDjpeg_add_quant_table_i := 5%positive.
Notation IDjpeg_add_quant_table_temp := 6%positive.
Notation IDjpeg_add_quant_table_basic_table := 7%positive.
Notation IDjpeg_add_quant_table_cinfo := 8%positive.
Notation IDjpeg_add_quant_table_force_baseline := 9%positive.
Notation IDjpeg_add_quant_table_scale_factor := 10%positive.
Notation IDjpeg_add_quant_table_which_tbl := 11%positive.
Definition jpeg_add_quant_table : graph := {|
  g_start := 1%positive;
  g_end := 17%positive;
  g_edges := (1%positive,(AAssign IDjpeg_add_quant_table_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDjpeg_add_quant_table__tmp2
             (Some (EVar IDjpeg_add_quant_table_which_tbl))),3%positive)::
             (3%positive,(AAssign IDjpeg_add_quant_table__tmp1
             (Some (EVar IDjpeg_add_quant_table_scale_factor))),4%positive)::
             (4%positive,(AAssign IDjpeg_add_quant_table__tmp
             (Some (EVar IDjpeg_add_quant_table_force_baseline))),5%positive)::
             (5%positive,AWeaken,6%positive)::(6%positive,ANone,8%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,AWeaken,10%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,12%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDjpeg_add_quant_table_i
             (Some (ENum (0)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table_i) s) <
             (eval (ENum (64)) s))%Z)),18%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table_i) s) >=
             (eval (ENum (64)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDjpeg_add_quant_table_temp None),
             20%positive)::(20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table_temp) s) <=
             (eval (ENum (0)) s))%Z)),23%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table_temp) s) >
             (eval (ENum (0)) s))%Z)),22%positive)::
             (22%positive,AWeaken,27%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AAssign IDjpeg_add_quant_table_temp
             (Some (ENum (1)))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,29%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,32%positive)::
             (29%positive,(AAssign IDjpeg_add_quant_table_temp None),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table__tmp) s) <>
             (eval (ENum (0)) s))%Z)),34%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table__tmp) s) =
             (eval (ENum (0)) s))%Z)),33%positive)::
             (33%positive,AWeaken,40%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table_temp) s) >
             (eval (ENum (255)) s))%Z)),37%positive)::
             (35%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_add_quant_table_temp) s) <=
             (eval (ENum (255)) s))%Z)),36%positive)::
             (36%positive,AWeaken,40%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AAssign IDjpeg_add_quant_table_temp
             (Some (ENum (255)))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDjpeg_add_quant_table_i
             (Some (EAdd (EVar IDjpeg_add_quant_table_i) (ENum (1))))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDjpeg_add_quant_table_z
             (Some (EAdd (ENum (1)) (EVar IDjpeg_add_quant_table_z)))),
             45%positive)::(45%positive,AWeaken,15%positive)::nil
|}.

Definition jpeg_add_quant_table_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 6%positive => (1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 7%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 9%positive => (1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 11%positive => (1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 12%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 13%positive => (1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 15%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -64 <= 0)%Z
    | 16%positive => (1 * (s IDjpeg_add_quant_table_i) + -64 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) + 64 <= 0)%Z
    | 17%positive => (-1 * (s IDjpeg_add_quant_table_i) + 64 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -64 <= 0)%Z
    | 18%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 19%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 20%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 21%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_temp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ 1 * (s IDjpeg_add_quant_table_temp) <= 0)%Z
    | 24%positive => (1 * (s IDjpeg_add_quant_table_temp) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 25%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ 1 * (s IDjpeg_add_quant_table_temp) + -1 <= 0 /\ -1 * (s IDjpeg_add_quant_table_temp) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_add_quant_table_temp) + 1 <= 0 /\ 1 * (s IDjpeg_add_quant_table_temp) + -1 <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 27%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_temp) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDjpeg_add_quant_table_temp) + 1 <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 29%positive => (-1 * (s IDjpeg_add_quant_table_temp) + 1 <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 30%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 31%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0 /\ 1 * (s IDjpeg_add_quant_table__tmp) <= 0 /\ -1 * (s IDjpeg_add_quant_table__tmp) <= 0)%Z
    | 34%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 35%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 36%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0 /\ 1 * (s IDjpeg_add_quant_table_temp) + -255 <= 0)%Z
    | 37%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_temp) + 256 <= 0)%Z
    | 38%positive => (-1 * (s IDjpeg_add_quant_table_temp) + 256 <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 39%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0 /\ 1 * (s IDjpeg_add_quant_table_temp) + -255 <= 0 /\ -1 * (s IDjpeg_add_quant_table_temp) + 255 <= 0)%Z
    | 40%positive => (-1 * (s IDjpeg_add_quant_table_i) <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -63 <= 0)%Z
    | 41%positive => (1 * (s IDjpeg_add_quant_table_i) + -63 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) <= 0)%Z
    | 42%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -64 <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDjpeg_add_quant_table_i) + 1 <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -64 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) <= 0)%Z
    | 44%positive => (-1 * (s IDjpeg_add_quant_table_z) <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -64 <= 0 /\ -1 * (s IDjpeg_add_quant_table_i) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDjpeg_add_quant_table_i) + 1 <= 0 /\ 1 * (s IDjpeg_add_quant_table_i) + -64 <= 0 /\ -1 * (s IDjpeg_add_quant_table_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_add_quant_table_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((64 # 1))%Q
    | 2%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 3%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 4%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 5%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 6%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 7%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 8%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 9%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 10%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 11%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 12%positive => ((64 # 1) + (s IDjpeg_add_quant_table_z))%Q
    | 13%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 14%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 15%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 16%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 17%positive => ((s IDjpeg_add_quant_table_z))%Q
    | 18%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 19%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 20%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 21%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 22%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 23%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 24%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 25%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 26%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 27%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 28%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 29%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 30%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 31%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 32%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 33%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 34%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 35%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 36%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 37%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 38%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 39%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 40%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 41%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(63 - (s IDjpeg_add_quant_table_i)))%Q
    | 42%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 43%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 44%positive => ((1 # 1) + (s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | 45%positive => ((s IDjpeg_add_quant_table_z)
                      + max0(64 - (s IDjpeg_add_quant_table_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_add_quant_table_hints (p : node) (s : state) := 
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
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDjpeg_add_quant_table_i)) (63
                                                                    - (s IDjpeg_add_quant_table_i)));
                      (*-1 0*) F_max0_ge_0 (63 - (s IDjpeg_add_quant_table_i))]
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
    | 28%positive => [(*-1 0*) F_max0_pre_decrement (64
                                                     - (s IDjpeg_add_quant_table_i)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_pre_decrement (64
                                                     - (s IDjpeg_add_quant_table_i)) (1)]
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
    | 45%positive => [(*-0.015625 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDjpeg_add_quant_table_i))) (F_check_ge ((s IDjpeg_add_quant_table_i)) (0));
                      (*0 0.015625*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDjpeg_add_quant_table_i)) (0))) (F_max0_ge_0 ((s IDjpeg_add_quant_table_i)))]
    | _ => []
  end.


Theorem jpeg_add_quant_table_ai_correct:
  forall s p' s', steps (g_start jpeg_add_quant_table) s (g_edges jpeg_add_quant_table) p' s' -> jpeg_add_quant_table_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_add_quant_table_pot_correct:
  forall s p' s',
    steps (g_start jpeg_add_quant_table) s (g_edges jpeg_add_quant_table) p' s' ->
    (jpeg_add_quant_table_pot (g_start jpeg_add_quant_table) s >= jpeg_add_quant_table_pot p' s')%Q.
Proof.
  check_lp jpeg_add_quant_table_ai_correct jpeg_add_quant_table_hints.
Qed.

