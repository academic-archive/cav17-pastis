Require Import pasta.Pasta.

Notation IDmad_bit_read_z := 1%positive.
Notation IDmad_bit_read__tmp := 2%positive.
Notation IDmad_bit_read__tmp1 := 3%positive.
Notation IDmad_bit_read_bitptr_dref_off10 := 4%positive.
Notation IDmad_bit_read_bitptr_dref_off8 := 5%positive.
Notation IDmad_bit_read_value := 6%positive.
Notation IDmad_bit_read_bitptr := 7%positive.
Notation IDmad_bit_read_len := 8%positive.
Definition mad_bit_read : graph := {|
  g_start := 1%positive;
  g_end := 45%positive;
  g_edges := (1%positive,(AAssign IDmad_bit_read_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDmad_bit_read_bitptr_dref_off10) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDmad_bit_read__tmp
             (Some (EVar IDmad_bit_read_len))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDmad_bit_read_bitptr_dref_off10) s) =
             (eval (ENum (8)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDmad_bit_read_bitptr_dref_off10) s) <>
             (eval (ENum (8)) s))%Z)),8%positive)::
             (8%positive,AWeaken,13%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDmad_bit_read_bitptr_dref_off8 None),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) < (eval (EVar IDmad_bit_read_bitptr_dref_off10) s))%Z)),
             39%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) >= (eval (EVar IDmad_bit_read_bitptr_dref_off10) s))%Z)),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDmad_bit_read_value None),16%positive)::
             (16%positive,(AAssign IDmad_bit_read__tmp
             (Some (ESub (EVar IDmad_bit_read__tmp)
             (EVar IDmad_bit_read_bitptr_dref_off10)))),17%positive)::
             (17%positive,(AAssign IDmad_bit_read_bitptr_dref_off10
             (Some (ENum (8)))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) >= (eval (ENum (8)) s))%Z)),32%positive)::
             (20%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) < (eval (ENum (8)) s))%Z)),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) > (eval (ENum (0)) s))%Z)),24%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDmad_bit_read__tmp)
             s) <= (eval (ENum (0)) s))%Z)),23%positive)::
             (23%positive,AWeaken,29%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDmad_bit_read_bitptr_dref_off8 None),
             26%positive)::
             (26%positive,(AAssign IDmad_bit_read_value None),27%positive)::
             (27%positive,(AAssign IDmad_bit_read_bitptr_dref_off10
             (Some (ESub (EVar IDmad_bit_read_bitptr_dref_off10)
             (EVar IDmad_bit_read__tmp)))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDmad_bit_read__tmp1
             (Some (EVar IDmad_bit_read_value))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,45%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AAssign IDmad_bit_read_value None),34%positive)::
             (34%positive,(AAssign IDmad_bit_read__tmp
             (Some (ESub (EVar IDmad_bit_read__tmp) (ENum (8))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDmad_bit_read_z (Some (EAdd (ENum (1))
             (EVar IDmad_bit_read_z)))),38%positive)::
             (38%positive,AWeaken,20%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDmad_bit_read_value None),41%positive)::
             (41%positive,(AAssign IDmad_bit_read_bitptr_dref_off10
             (Some (ESub (EVar IDmad_bit_read_bitptr_dref_off10)
             (EVar IDmad_bit_read__tmp)))),42%positive)::
             (42%positive,(AAssign IDmad_bit_read__tmp1
             (Some (EVar IDmad_bit_read_value))),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::nil
|}.

Definition mad_bit_read_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0)%Z
    | 4%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0)%Z
    | 6%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0)%Z
    | 8%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 9%positive => (1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 10%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0)%Z
    | 11%positive => (1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 12%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 14%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0)%Z
    | 15%positive => (-1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 16%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0)%Z
    | 17%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 19%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 21%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0)%Z
    | 22%positive => (1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 23%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 24%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDmad_bit_read__tmp) + 1 <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 26%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read__tmp) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDmad_bit_read__tmp) + 1 <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 28%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read__tmp) + 1 <= 0 /\ 1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 29%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 30%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ 1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 31%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read__tmp)+ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ 1 * (s IDmad_bit_read__tmp) + -7 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 32%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp) + 8 <= 0)%Z
    | 33%positive => (-1 * (s IDmad_bit_read__tmp) + 8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0)%Z
    | 34%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp) + 8 <= 0)%Z
    | 35%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 36%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 37%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ -1 * (s IDmad_bit_read__tmp) <= 0)%Z
    | 38%positive => (-1 * (s IDmad_bit_read__tmp) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 8 <= 0 /\ 1 * (s IDmad_bit_read_bitptr_dref_off10) + -8 <= 0 /\ -1 * (s IDmad_bit_read_z) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0)%Z
    | 40%positive => (1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | 41%positive => (-1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ 1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0)%Z
    | 42%positive => (1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ 1 * (s IDmad_bit_read_z) <= 0)%Z
    | 44%positive => (1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDmad_bit_read_bitptr_dref_off10) + 1 <= 0 /\ -1 * (s IDmad_bit_read__tmp)+ -1 * (s IDmad_bit_read_bitptr_dref_off10) <= 0 /\ -1 * (s IDmad_bit_read_z) <= 0)%Z
    | _ => False
  end.

Definition mad_bit_read_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0((s IDmad_bit_read_len)))%Q
    | 2%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read_len)))%Q
    | 3%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read_len)))%Q
    | 4%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read_len)))%Q
    | 5%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read_len)))%Q
    | 6%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 7%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 8%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 9%positive => ((s IDmad_bit_read_z)
                     + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 10%positive => ((s IDmad_bit_read_z)
                      + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 11%positive => ((s IDmad_bit_read_z)
                      + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 12%positive => ((s IDmad_bit_read_z)
                      + (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 13%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 14%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 15%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      - (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      + max0((s IDmad_bit_read_z)))%Q
    | 16%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      - (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      + max0((s IDmad_bit_read_z)))%Q
    | 17%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + max0((s IDmad_bit_read_z)))%Q
    | 18%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 19%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 20%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 21%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 22%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (s IDmad_bit_read_z)
                      - (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 23%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (s IDmad_bit_read_z)
                      - (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 24%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (s IDmad_bit_read_z)
                      - (1 # 8) * max0((s IDmad_bit_read__tmp)))%Q
    | 25%positive => ((s IDmad_bit_read_z))%Q
    | 26%positive => ((s IDmad_bit_read_z))%Q
    | 27%positive => ((s IDmad_bit_read_z))%Q
    | 28%positive => ((s IDmad_bit_read_z))%Q
    | 29%positive => ((s IDmad_bit_read_z))%Q
    | 30%positive => ((s IDmad_bit_read_z))%Q
    | 31%positive => ((s IDmad_bit_read_z))%Q
    | 32%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 33%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 34%positive => ((1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 35%positive => ((1 # 1) + (1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 36%positive => ((1 # 1) + (1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 37%positive => ((1 # 1) + (1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10))
                      + max0((s IDmad_bit_read_z)))%Q
    | 38%positive => ((1 # 1) + (1 # 8) * (s IDmad_bit_read__tmp)
                      + (1 # 8) * (s IDmad_bit_read_bitptr_dref_off10)
                      + max0(-1 + (s IDmad_bit_read_z))
                      - (1 # 8) * max0((s IDmad_bit_read_bitptr_dref_off10)))%Q
    | 39%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 40%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 41%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 42%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 43%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 44%positive => ((1 # 8) * max0((s IDmad_bit_read__tmp))
                      + max0((s IDmad_bit_read_z)))%Q
    | 45%positive => ((s IDmad_bit_read_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition mad_bit_read_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read_z)) (0))) (F_max0_ge_0 ((s IDmad_bit_read_z)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read_z)) (0))) (F_max0_ge_0 ((s IDmad_bit_read_z)))]
    | 13%positive => []
    | 14%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDmad_bit_read_bitptr_dref_off10))) (F_check_ge (0) (0));
                      (*2.2175e-11 0.125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read_bitptr_dref_off10)) (0))) (F_max0_ge_0 ((s IDmad_bit_read_bitptr_dref_off10)));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmad_bit_read__tmp))) (F_check_ge ((s IDmad_bit_read__tmp)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-0.125 0*) F_max0_ge_0 ((s IDmad_bit_read__tmp));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmad_bit_read_z))) (F_check_ge ((s IDmad_bit_read_z)) (0));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read_bitptr_dref_off10)) (0))) (F_max0_ge_0 ((s IDmad_bit_read_bitptr_dref_off10)))]
    | 22%positive => []
    | 23%positive => [(*1e-12 0.125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read__tmp)) (0))) (F_max0_ge_0 ((s IDmad_bit_read__tmp)))]
    | 24%positive => [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read__tmp)) (0))) (F_max0_ge_0 ((s IDmad_bit_read__tmp)))]
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
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDmad_bit_read_z)) (0))) (F_max0_ge_0 ((s IDmad_bit_read_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDmad_bit_read_z))) (F_check_ge (-1
                                                                    + (s IDmad_bit_read_z)) (0))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-0.125 0*) F_max0_monotonic (F_check_ge ((s IDmad_bit_read__tmp)) (-8
                                                                    + (s IDmad_bit_read__tmp)));
                      (*-0.125 0*) F_max0_ge_0 (-8 + (s IDmad_bit_read__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDmad_bit_read_z))) (F_check_ge ((s IDmad_bit_read_z)) (0))]
    | 45%positive => []
    | _ => []
  end.


Theorem mad_bit_read_ai_correct:
  forall s p' s', steps (g_start mad_bit_read) s (g_edges mad_bit_read) p' s' -> mad_bit_read_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mad_bit_read_pot_correct:
  forall s p' s',
    steps (g_start mad_bit_read) s (g_edges mad_bit_read) p' s' ->
    (mad_bit_read_pot (g_start mad_bit_read) s >= mad_bit_read_pot p' s')%Q.
Proof.
  check_lp mad_bit_read_ai_correct mad_bit_read_hints.
Qed.

