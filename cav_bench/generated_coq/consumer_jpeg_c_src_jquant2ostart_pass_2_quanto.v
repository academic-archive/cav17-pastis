Require Import pasta.Pasta.

Notation IDstart_pass_2_quant_z := 1%positive.
Notation IDstart_pass_2_quant__tmp := 2%positive.
Notation IDstart_pass_2_quant_arraysize := 3%positive.
Notation IDstart_pass_2_quant_i := 4%positive.
Notation IDstart_pass_2_quant_cinfo := 5%positive.
Notation IDstart_pass_2_quant_is_pre_scan := 6%positive.
Definition start_pass_2_quant : graph := {|
  g_start := 1%positive;
  g_end := 49%positive;
  g_edges := (1%positive,(AAssign IDstart_pass_2_quant_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDstart_pass_2_quant__tmp
             (Some (EVar IDstart_pass_2_quant_is_pre_scan))),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,6%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,8%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant__tmp) s) <>
             (eval (ENum (0)) s))%Z)),37%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant__tmp) s) =
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,12%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,13%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDstart_pass_2_quant_i None),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant_i) s) <
             (eval (ENum (1)) s))%Z)),17%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant_i) s) >=
             (eval (ENum (1)) s))%Z)),16%positive)::
             (16%positive,AWeaken,20%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant_i) s) >
             (eval (ENum (256)) s))%Z)),22%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant_i) s) <=
             (eval (ENum (256)) s))%Z)),21%positive)::
             (21%positive,AWeaken,25%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (25%positive,ANone,35%positive)::
             (26%positive,(AAssign IDstart_pass_2_quant_arraysize None),
             27%positive)::(27%positive,AWeaken,28%positive)::
             (28%positive,ANone,30%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,32%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (32%positive,ANone,34%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,AWeaken,40%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,ANone,42%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,49%positive)::
             (42%positive,(AAssign IDstart_pass_2_quant_i (Some (ENum (0)))),
             43%positive)::(43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant_i) s) <
             (eval (ENum (32)) s))%Z)),50%positive)::
             (45%positive,(AGuard
             (fun s => ((eval (EVar IDstart_pass_2_quant_i) s) >=
             (eval (ENum (32)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,ANone,52%positive)::
             (52%positive,(AAssign IDstart_pass_2_quant_i
             (Some (EAdd (EVar IDstart_pass_2_quant_i) (ENum (1))))),
             53%positive)::(53%positive,ANone,54%positive)::
             (54%positive,ANone,55%positive)::
             (55%positive,(AAssign IDstart_pass_2_quant_z
             (Some (EAdd (ENum (1)) (EVar IDstart_pass_2_quant_z)))),
             56%positive)::(56%positive,AWeaken,45%positive)::nil
|}.

Definition start_pass_2_quant_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 3%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 4%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 5%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 6%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 7%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 8%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 9%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 10%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 11%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 12%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 13%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 14%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 15%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 16%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 1 <= 0)%Z
    | 17%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) <= 0)%Z
    | 18%positive => (1 * (s IDstart_pass_2_quant_i) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 19%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) <= 0)%Z
    | 20%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 21%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -256 <= 0)%Z
    | 22%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 257 <= 0)%Z
    | 23%positive => (-1 * (s IDstart_pass_2_quant_i) + 257 <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 24%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 257 <= 0)%Z
    | 25%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 26%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 27%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 28%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 29%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 30%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 31%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 32%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 33%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 34%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 35%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant__tmp) <= 0)%Z
    | 36%positive => (-1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ 1 * (s IDstart_pass_2_quant__tmp) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 37%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 38%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 39%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 40%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 41%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 42%positive => (1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 43%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) <= 0)%Z
    | 44%positive => (-1 * (s IDstart_pass_2_quant_i) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) <= 0 /\ 1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 45%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -32 <= 0)%Z
    | 46%positive => (1 * (s IDstart_pass_2_quant_i) + -32 <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 32 <= 0)%Z
    | 47%positive => (-1 * (s IDstart_pass_2_quant_i) + 32 <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -32 <= 0)%Z
    | 48%positive => (1 * (s IDstart_pass_2_quant_i) + -32 <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 32 <= 0)%Z
    | 49%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 50%positive => (-1 * (s IDstart_pass_2_quant_i) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -31 <= 0)%Z
    | 51%positive => (1 * (s IDstart_pass_2_quant_i) + -31 <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) <= 0)%Z
    | 52%positive => (-1 * (s IDstart_pass_2_quant_i) <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -31 <= 0)%Z
    | 53%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 1 <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -32 <= 0)%Z
    | 54%positive => (1 * (s IDstart_pass_2_quant_i) + -32 <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 1 <= 0 /\ -1 * (s IDstart_pass_2_quant_z) <= 0)%Z
    | 55%positive => (-1 * (s IDstart_pass_2_quant_z) <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 1 <= 0 /\ 1 * (s IDstart_pass_2_quant_i) + -32 <= 0)%Z
    | 56%positive => (1 * (s IDstart_pass_2_quant_i) + -32 <= 0 /\ -1 * (s IDstart_pass_2_quant_i) + 1 <= 0 /\ -1 * (s IDstart_pass_2_quant_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition start_pass_2_quant_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((32 # 1))%Q
    | 2%positive => ((32 # 1))%Q
    | 3%positive => ((32 # 1))%Q
    | 4%positive => ((32 # 1))%Q
    | 5%positive => ((32 # 1))%Q
    | 6%positive => ((32 # 1))%Q
    | 7%positive => ((32 # 1))%Q
    | 8%positive => ((32 # 1))%Q
    | 9%positive => ((32 # 1))%Q
    | 10%positive => ((32 # 1))%Q
    | 11%positive => ((32 # 1))%Q
    | 12%positive => ((32 # 1))%Q
    | 13%positive => ((32 # 1))%Q
    | 14%positive => ((32 # 1))%Q
    | 15%positive => ((32 # 1))%Q
    | 16%positive => ((32 # 1))%Q
    | 17%positive => ((32 # 1))%Q
    | 18%positive => ((32 # 1))%Q
    | 19%positive => ((32 # 1))%Q
    | 20%positive => ((32 # 1))%Q
    | 21%positive => ((32 # 1))%Q
    | 22%positive => ((32 # 1))%Q
    | 23%positive => ((32 # 1))%Q
    | 24%positive => ((32 # 1))%Q
    | 25%positive => ((32 # 1))%Q
    | 26%positive => ((32 # 1))%Q
    | 27%positive => ((32 # 1))%Q
    | 28%positive => ((32 # 1))%Q
    | 29%positive => ((32 # 1))%Q
    | 30%positive => ((32 # 1))%Q
    | 31%positive => ((32 # 1))%Q
    | 32%positive => ((32 # 1))%Q
    | 33%positive => ((32 # 1))%Q
    | 34%positive => ((32 # 1))%Q
    | 35%positive => ((32 # 1))%Q
    | 36%positive => ((32 # 1))%Q
    | 37%positive => ((32 # 1))%Q
    | 38%positive => ((32 # 1))%Q
    | 39%positive => ((32 # 1))%Q
    | 40%positive => ((32 # 1))%Q
    | 41%positive => ((32 # 1))%Q
    | 42%positive => ((32 # 1))%Q
    | 43%positive => (max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 44%positive => (max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 45%positive => ((s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 46%positive => ((s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 47%positive => ((s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 48%positive => ((s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 49%positive => ((s IDstart_pass_2_quant_z))%Q
    | 50%positive => ((s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 51%positive => ((1 # 1) + (s IDstart_pass_2_quant_z)
                      + max0(31 - (s IDstart_pass_2_quant_i)))%Q
    | 52%positive => ((1 # 1) + (s IDstart_pass_2_quant_z)
                      + max0(31 - (s IDstart_pass_2_quant_i)))%Q
    | 53%positive => ((1 # 1) + (s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 54%positive => ((1 # 1) + (s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 55%positive => ((1 # 1) + (s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | 56%positive => ((s IDstart_pass_2_quant_z)
                      + max0(32 - (s IDstart_pass_2_quant_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition start_pass_2_quant_hints (p : node) (s : state) := 
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
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
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
    | 41%positive => [(*-32 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDstart_pass_2_quant_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDstart_pass_2_quant_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDstart_pass_2_quant_z)))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDstart_pass_2_quant_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDstart_pass_2_quant_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDstart_pass_2_quant_z)))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (32
                                                             - (s IDstart_pass_2_quant_i)) (31
                                                                    - (s IDstart_pass_2_quant_i)));
                      (*-1 0*) F_max0_ge_0 (31 - (s IDstart_pass_2_quant_i))]
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_max0_pre_decrement (32
                                                     - (s IDstart_pass_2_quant_i)) (1)]
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | _ => []
  end.


Theorem start_pass_2_quant_ai_correct:
  forall s p' s', steps (g_start start_pass_2_quant) s (g_edges start_pass_2_quant) p' s' -> start_pass_2_quant_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem start_pass_2_quant_pot_correct:
  forall s p' s',
    steps (g_start start_pass_2_quant) s (g_edges start_pass_2_quant) p' s' ->
    (start_pass_2_quant_pot (g_start start_pass_2_quant) s >= start_pass_2_quant_pot p' s')%Q.
Proof.
  check_lp start_pass_2_quant_ai_correct start_pass_2_quant_hints.
Qed.

