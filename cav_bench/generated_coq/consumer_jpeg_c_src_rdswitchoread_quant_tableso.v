Require Import pasta.Pasta.

Notation IDread_quant_tables_z := 1%positive.
Notation IDread_quant_tables__tmp := 2%positive.
Notation IDread_quant_tables__tmp1 := 3%positive.
Notation IDread_quant_tables__tmp2 := 4%positive.
Notation IDread_quant_tables_i := 5%positive.
Notation IDread_quant_tables_tblno := 6%positive.
Notation IDread_quant_tables_cinfo := 7%positive.
Notation IDread_quant_tables_filename := 8%positive.
Notation IDread_quant_tables_force_baseline := 9%positive.
Notation IDread_quant_tables_scale_factor := 10%positive.
Definition read_quant_tables : graph := {|
  g_start := 1%positive;
  g_end := 49%positive;
  g_edges := (1%positive,(AAssign IDread_quant_tables_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDread_quant_tables__tmp2
             (Some (EVar IDread_quant_tables_scale_factor))),3%positive)::
             (3%positive,(AAssign IDread_quant_tables__tmp1
             (Some (EVar IDread_quant_tables_force_baseline))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,46%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDread_quant_tables_tblno
             (Some (ENum (0)))),7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,18%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,15%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDread_quant_tables__tmp
             (Some (ENum (1)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,49%positive)::
             (15%positive,(AAssign IDread_quant_tables__tmp
             (Some (ENum (0)))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,49%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDread_quant_tables_tblno) s) >=
             (eval (ENum (4)) s))%Z)),42%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDread_quant_tables_tblno) s) <
             (eval (ENum (4)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDread_quant_tables_i (Some (ENum (1)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDread_quant_tables_i) s) <
             (eval (ENum (64)) s))%Z)),31%positive)::
             (24%positive,(AGuard
             (fun s => ((eval (EVar IDread_quant_tables_i) s) >=
             (eval (ENum (64)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDread_quant_tables_tblno
             (Some (EAdd (EVar IDread_quant_tables_tblno) (ENum (1))))),
             27%positive)::(27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDread_quant_tables_z
             (Some (EAdd (ENum (1)) (EVar IDread_quant_tables_z)))),
             30%positive)::(30%positive,AWeaken,9%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,36%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDread_quant_tables__tmp
             (Some (ENum (0)))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,49%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDread_quant_tables_i
             (Some (EAdd (EVar IDread_quant_tables_i) (ENum (1))))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDread_quant_tables_z
             (Some (EAdd (ENum (1)) (EVar IDread_quant_tables_z)))),
             41%positive)::(41%positive,AWeaken,24%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AAssign IDread_quant_tables__tmp
             (Some (ENum (0)))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,49%positive)::
             (46%positive,(AAssign IDread_quant_tables__tmp
             (Some (ENum (0)))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,49%positive)::nil
|}.

Definition read_quant_tables_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_z) <= 0)%Z
    | 4%positive => (1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 5%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_z) <= 0)%Z
    | 6%positive => (1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 7%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 8%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 9%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 10%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 11%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 12%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 13%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables__tmp) + -1 <= 0 /\ -1 * (s IDread_quant_tables__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDread_quant_tables__tmp) + 1 <= 0 /\ 1 * (s IDread_quant_tables__tmp) + -1 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 15%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 16%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables__tmp) <= 0)%Z
    | 17%positive => (-1 * (s IDread_quant_tables__tmp) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 18%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 19%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 20%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_tblno) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDread_quant_tables_tblno) + -3 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 22%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_tblno) + -3 <= 0 /\ 1 * (s IDread_quant_tables_i) + -1 <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDread_quant_tables_i) + 1 <= 0 /\ 1 * (s IDread_quant_tables_i) + -1 <= 0 /\ 1 * (s IDread_quant_tables_tblno) + -3 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 24%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0)%Z
    | 25%positive => (1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 64 <= 0)%Z
    | 26%positive => (-1 * (s IDread_quant_tables_i) + 64 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0)%Z
    | 27%positive => (1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 64 <= 0 /\ -1 * (s IDread_quant_tables_tblno) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDread_quant_tables_tblno) + 1 <= 0 /\ -1 * (s IDread_quant_tables_i) + 64 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0)%Z
    | 29%positive => (1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 64 <= 0 /\ -1 * (s IDread_quant_tables_tblno) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDread_quant_tables_tblno) + 1 <= 0 /\ -1 * (s IDread_quant_tables_i) + 64 <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_z) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -63 <= 0)%Z
    | 32%positive => (1 * (s IDread_quant_tables_i) + -63 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 33%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -63 <= 0)%Z
    | 34%positive => (1 * (s IDread_quant_tables_i) + -63 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables__tmp) <= 0)%Z
    | 35%positive => (-1 * (s IDread_quant_tables__tmp) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -63 <= 0)%Z
    | 36%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -63 <= 0)%Z
    | 37%positive => (1 * (s IDread_quant_tables_i) + -63 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_i) + 1 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 38%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_i) + 2 <= 0)%Z
    | 39%positive => (-1 * (s IDread_quant_tables_i) + 2 <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0)%Z
    | 40%positive => (-1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_i) + 2 <= 0)%Z
    | 41%positive => (-1 * (s IDread_quant_tables_i) + 2 <= 0 /\ 1 * (s IDread_quant_tables_i) + -64 <= 0 /\ -1 * (s IDread_quant_tables_tblno) <= 0 /\ -1 * (s IDread_quant_tables_z) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) + 4 <= 0)%Z
    | 43%positive => (-1 * (s IDread_quant_tables_tblno) + 4 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 44%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_tblno) + 4 <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables__tmp) <= 0)%Z
    | 45%positive => (-1 * (s IDread_quant_tables__tmp) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables_tblno) + 4 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 46%positive => (1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 47%positive => (-1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables_z) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ -1 * (s IDread_quant_tables__tmp) <= 0)%Z
    | 48%positive => (-1 * (s IDread_quant_tables__tmp) <= 0 /\ 1 * (s IDread_quant_tables__tmp) <= 0 /\ 1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0)%Z
    | 49%positive => (1 * (s IDread_quant_tables__tmp) + -1 <= 0 /\ -1 * (s IDread_quant_tables_z) <= 0 /\ -1 * (s IDread_quant_tables__tmp) <= 0)%Z
    | _ => False
  end.

Definition read_quant_tables_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 3%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 4%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 5%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 6%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 7%positive => ((s IDread_quant_tables_z)
                     + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 8%positive => ((s IDread_quant_tables_z)
                     + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 9%positive => ((s IDread_quant_tables_z)
                     + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 10%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 11%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 12%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 13%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 14%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 15%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 16%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 17%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 18%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 19%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 20%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 21%positive => ((64 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno)))%Q
    | 22%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 23%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 24%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 25%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 26%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 27%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 28%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 29%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 30%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 31%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 32%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(63 - (s IDread_quant_tables_i)))%Q
    | 33%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(63 - (s IDread_quant_tables_i)))%Q
    | 34%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(63 - (s IDread_quant_tables_i)))%Q
    | 35%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(63 - (s IDread_quant_tables_i)))%Q
    | 36%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(63 - (s IDread_quant_tables_i)))%Q
    | 37%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(63 - (s IDread_quant_tables_i)))%Q
    | 38%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 39%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 40%positive => ((2 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 41%positive => ((1 # 1) + (s IDread_quant_tables_z)
                      + (64 # 1) * max0(3 - (s IDread_quant_tables_tblno))
                      + max0(64 - (s IDread_quant_tables_i)))%Q
    | 42%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 43%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 44%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 45%positive => ((s IDread_quant_tables_z)
                      + (64 # 1) * max0(4 - (s IDread_quant_tables_tblno)))%Q
    | 46%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 47%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 48%positive => ((256 # 1) + (s IDread_quant_tables_z))%Q
    | 49%positive => ((s IDread_quant_tables_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_quant_tables_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-64 0*) F_max0_monotonic (F_check_ge (4
                                                              - (s IDread_quant_tables_tblno)) (3
                                                                    - (s IDread_quant_tables_tblno)));
                      (*-64 0*) F_max0_ge_0 (3
                                             - (s IDread_quant_tables_tblno))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-64 0*) F_max0_monotonic (F_check_ge (4
                                                              - (s IDread_quant_tables_tblno)) (3
                                                                    - (s IDread_quant_tables_tblno)));
                      (*-64 0*) F_max0_ge_0 (3
                                             - (s IDread_quant_tables_tblno))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*0 64*) F_max0_pre_decrement (4
                                                     - (s IDread_quant_tables_tblno)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (64
                                                                 - (s IDread_quant_tables_i))) (F_check_ge (0) (0))]
    | 31%positive => [(*0 1*) F_max0_pre_decrement (64
                                                    - (s IDread_quant_tables_i)) (1)]
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-64 0*) F_max0_ge_0 (3
                                             - (s IDread_quant_tables_tblno));
                      (*-1.03226 0*) F_max0_ge_0 (63
                                                  - (s IDread_quant_tables_i));
                      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (63
                                                                    - (s IDread_quant_tables_i)) (0))) (F_max0_ge_0 (63
                                                                    - (s IDread_quant_tables_i)));
                      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDread_quant_tables_i))) (F_check_ge (0) (0));
                      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDread_quant_tables_i)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDread_quant_tables_i)))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => [(*-64 0*) F_max0_monotonic (F_check_ge (4
                                                              - (s IDread_quant_tables_tblno)) (3
                                                                    - (s IDread_quant_tables_tblno)));
                      (*-64 0*) F_max0_ge_0 (3
                                             - (s IDread_quant_tables_tblno))]
    | 46%positive => []
    | 47%positive => []
    | 48%positive => [(*-256 0*) F_one]
    | 49%positive => []
    | _ => []
  end.


Theorem read_quant_tables_ai_correct:
  forall s p' s', steps (g_start read_quant_tables) s (g_edges read_quant_tables) p' s' -> read_quant_tables_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_quant_tables_pot_correct:
  forall s p' s',
    steps (g_start read_quant_tables) s (g_edges read_quant_tables) p' s' ->
    (read_quant_tables_pot (g_start read_quant_tables) s >= read_quant_tables_pot p' s')%Q.
Proof.
  check_lp read_quant_tables_ai_correct read_quant_tables_hints.
Qed.

