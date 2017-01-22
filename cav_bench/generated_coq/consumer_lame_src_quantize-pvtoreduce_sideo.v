Require Import pasta.Pasta.

Notation IDreduce_side_z := 1%positive.
Notation IDreduce_side__tmp := 2%positive.
Notation IDreduce_side_ch := 3%positive.
Notation IDreduce_side_max_bits := 4%positive.
Notation IDreduce_side_numchn := 5%positive.
Notation IDreduce_side_targ_bits_dref_off0 := 6%positive.
Notation IDreduce_side_targ_bits_dref_off4 := 7%positive.
Notation IDreduce_side_mean_bits := 8%positive.
Notation IDreduce_side_ms_ener_ratio := 9%positive.
Notation IDreduce_side_targ_bits := 10%positive.
Definition reduce_side : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDreduce_side_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDreduce_side__tmp
             (Some (EVar IDreduce_side_mean_bits))),3%positive)::
             (3%positive,(AAssign IDreduce_side_numchn (Some (ENum (2)))),
             4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,ANone,7%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,9%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDreduce_side_targ_bits_dref_off4) s) >=
             (eval (ENum (125)) s))%Z)),11%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDreduce_side_targ_bits_dref_off4) s) <
             (eval (ENum (125)) s))%Z)),10%positive)::
             (10%positive,AWeaken,20%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,16%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDreduce_side_targ_bits_dref_off0
             (Some (EAdd (EVar IDreduce_side_targ_bits_dref_off0)
             (ESub (EVar IDreduce_side_targ_bits_dref_off4) (ENum (125)))))),
             14%positive)::
             (14%positive,(AAssign IDreduce_side_targ_bits_dref_off4
             (Some (ENum (125)))),15%positive)::
             (15%positive,ANone,19%positive)::
             (16%positive,(AAssign IDreduce_side_targ_bits_dref_off0 None),
             17%positive)::
             (17%positive,(AAssign IDreduce_side_targ_bits_dref_off4 None),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDreduce_side_ch (Some (ENum (0)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDreduce_side_ch)
             s) < (eval (EVar IDreduce_side_numchn) s))%Z)),26%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDreduce_side_ch)
             s) >= (eval (EVar IDreduce_side_numchn) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,34%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDreduce_side_max_bits None),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => True)),33%positive)::
             (31%positive,(AGuard (fun s => True)),32%positive)::
             (32%positive,AWeaken,36%positive)::
             (33%positive,AWeaken,35%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDreduce_side_ch
             (Some (EAdd (EVar IDreduce_side_ch) (ENum (1))))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDreduce_side_z (Some (EAdd (ENum (1))
             (EVar IDreduce_side_z)))),41%positive)::
             (41%positive,AWeaken,23%positive)::nil
|}.

Definition reduce_side_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0)%Z
    | 3%positive => (-1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0)%Z
    | 4%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 5%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0)%Z
    | 6%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 7%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 8%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0)%Z
    | 9%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 10%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_targ_bits_dref_off4) + -124 <= 0)%Z
    | 11%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0)%Z
    | 12%positive => (-1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 13%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0)%Z
    | 14%positive => (-1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 15%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_targ_bits_dref_off4) + -125 <= 0 /\ -1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0)%Z
    | 16%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0)%Z
    | 17%positive => (-1 * (s IDreduce_side_targ_bits_dref_off4) + 125 <= 0 /\ 1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 18%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0)%Z
    | 19%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0)%Z
    | 20%positive => (-1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0)%Z
    | 21%positive => (1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0)%Z
    | 22%positive => (-1 * (s IDreduce_side_ch) <= 0 /\ 1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_z) <= 0)%Z
    | 23%positive => (-1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0)%Z
    | 24%positive => (1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch)+ 1 * (s IDreduce_side_numchn) <= 0)%Z
    | 25%positive => (-1 * (s IDreduce_side_ch)+ 1 * (s IDreduce_side_numchn) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0)%Z
    | 26%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 27%positive => (1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0)%Z
    | 28%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 29%positive => (1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0)%Z
    | 30%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 31%positive => (1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0)%Z
    | 32%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0)%Z
    | 36%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0)%Z
    | 37%positive => (1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) + 1 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_ch) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0)%Z
    | 38%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0 /\ -1 * (s IDreduce_side_ch) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDreduce_side_ch) + 1 <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0)%Z
    | 40%positive => (1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ -1 * (s IDreduce_side_z) <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0 /\ -1 * (s IDreduce_side_ch) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDreduce_side_ch) + 1 <= 0 /\ 1 * (s IDreduce_side_ch)+ -1 * (s IDreduce_side_numchn) <= 0 /\ -1 * (s IDreduce_side_numchn) + 2 <= 0 /\ 1 * (s IDreduce_side_numchn) + -2 <= 0 /\ -1 * (s IDreduce_side_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition reduce_side_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1))%Q
    | 2%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 3%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 4%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 5%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 6%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 7%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 8%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 9%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 10%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 11%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 12%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 13%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 14%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 15%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 16%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 17%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 18%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 19%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 20%positive => ((2 # 1) + (s IDreduce_side_z))%Q
    | 21%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 22%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 23%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 24%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 25%positive => ((s IDreduce_side_z))%Q
    | 26%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 27%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 28%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 29%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 30%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 31%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 32%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 33%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 34%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 35%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 36%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 37%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 38%positive => ((3 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 39%positive => ((3 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 40%positive => ((3 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | 41%positive => ((2 # 1) - (s IDreduce_side_ch) + (s IDreduce_side_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition reduce_side_hints (p : node) (s : state) := 
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
    | 24%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                 - (s IDreduce_side_ch))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDreduce_side_ch)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDreduce_side_ch)))]
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
    | 41%positive => []
    | _ => []
  end.


Theorem reduce_side_ai_correct:
  forall s p' s', steps (g_start reduce_side) s (g_edges reduce_side) p' s' -> reduce_side_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem reduce_side_pot_correct:
  forall s p' s',
    steps (g_start reduce_side) s (g_edges reduce_side) p' s' ->
    (reduce_side_pot (g_start reduce_side) s >= reduce_side_pot p' s')%Q.
Proof.
  check_lp reduce_side_ai_correct reduce_side_hints.
Qed.

