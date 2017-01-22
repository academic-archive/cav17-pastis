Require Import pasta.Pasta.

Notation IDemit_dqt_z := 1%positive.
Notation IDemit_dqt__tmp := 2%positive.
Notation IDemit_dqt_i := 3%positive.
Notation IDemit_dqt_prec := 4%positive.
Notation IDemit_dqt_qval := 5%positive.
Notation IDemit_dqt_cinfo := 6%positive.
Notation IDemit_dqt_index := 7%positive.
Definition emit_dqt : graph := {|
  g_start := 1%positive;
  g_end := 34%positive;
  g_edges := (1%positive,(AAssign IDemit_dqt_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDemit_dqt__tmp
             (Some (EVar IDemit_dqt_index))),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,5%positive)::
             (4%positive,ANone,6%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDemit_dqt_prec (Some (ENum (0)))),
             7%positive)::
             (7%positive,(AAssign IDemit_dqt_i (Some (ENum (0)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDemit_dqt_i) s) <
             (eval (ENum (64)) s))%Z)),35%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDemit_dqt_i) s) >=
             (eval (ENum (64)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,33%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDemit_dqt_i (Some (ENum (0)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDemit_dqt_i) s) <
             (eval (ENum (64)) s))%Z)),20%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDemit_dqt_i) s) >=
             (eval (ENum (64)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,34%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDemit_dqt_qval None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDemit_dqt_prec)
             s) <> (eval (ENum (0)) s))%Z)),25%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDemit_dqt_prec)
             s) = (eval (ENum (0)) s))%Z)),24%positive)::
             (24%positive,AWeaken,27%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDemit_dqt_i
             (Some (EAdd (EVar IDemit_dqt_i) (ENum (1))))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDemit_dqt_z (Some (EAdd (ENum (1))
             (EVar IDemit_dqt_z)))),32%positive)::
             (32%positive,AWeaken,16%positive)::
             (33%positive,AWeaken,34%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (36%positive,ANone,39%positive)::
             (37%positive,(AAssign IDemit_dqt_prec (Some (ENum (1)))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDemit_dqt_i
             (Some (EAdd (EVar IDemit_dqt_i) (ENum (1))))),41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDemit_dqt_z (Some (EAdd (ENum (1))
             (EVar IDemit_dqt_z)))),44%positive)::
             (44%positive,AWeaken,10%positive)::nil
|}.

Definition emit_dqt_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0)%Z
    | 3%positive => (-1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_z) <= 0)%Z
    | 4%positive => (1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0)%Z
    | 5%positive => (-1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_z) <= 0)%Z
    | 6%positive => (1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0)%Z
    | 7%positive => (-1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 8%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0)%Z
    | 9%positive => (-1 * (s IDemit_dqt_i) <= 0 /\ 1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 10%positive => (-1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0)%Z
    | 11%positive => (1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) + 64 <= 0)%Z
    | 12%positive => (-1 * (s IDemit_dqt_i) + 64 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0)%Z
    | 13%positive => (1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) + 64 <= 0)%Z
    | 14%positive => (-1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0)%Z
    | 15%positive => (-1 * (s IDemit_dqt_i) <= 0 /\ 1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0)%Z
    | 16%positive => (-1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0)%Z
    | 17%positive => (1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) + 64 <= 0)%Z
    | 18%positive => (-1 * (s IDemit_dqt_i) + 64 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0)%Z
    | 19%positive => (1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) + 64 <= 0)%Z
    | 20%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0)%Z
    | 21%positive => (1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 22%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0)%Z
    | 23%positive => (1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 24%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0 /\ 1 * (s IDemit_dqt_prec) <= 0)%Z
    | 25%positive => (-1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_prec) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDemit_dqt_prec) + 1 <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0)%Z
    | 27%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0)%Z
    | 28%positive => (1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 29%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDemit_dqt_i) + 1 <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 31%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDemit_dqt_i) + 1 <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) + 1 <= 0)%Z
    | 33%positive => (1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) + 64 <= 0)%Z
    | 34%positive => (-1 * (s IDemit_dqt_i) + 64 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0)%Z
    | 35%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0)%Z
    | 36%positive => (1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 37%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0)%Z
    | 38%positive => (1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ 1 * (s IDemit_dqt_prec) + -1 <= 0 /\ -1 * (s IDemit_dqt_prec) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -63 <= 0)%Z
    | 40%positive => (1 * (s IDemit_dqt_i) + -63 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_i) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 41%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_i) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDemit_dqt_i) + 1 <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0)%Z
    | 43%positive => (-1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_i) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDemit_dqt_i) + 1 <= 0 /\ 1 * (s IDemit_dqt_i) + -64 <= 0 /\ -1 * (s IDemit_dqt_prec) <= 0 /\ -1 * (s IDemit_dqt_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition emit_dqt_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((128 # 1))%Q
    | 2%positive => ((128 # 1) + (s IDemit_dqt_z))%Q
    | 3%positive => ((128 # 1) + (s IDemit_dqt_z))%Q
    | 4%positive => ((128 # 1) + (s IDemit_dqt_z))%Q
    | 5%positive => ((128 # 1) + (s IDemit_dqt_z))%Q
    | 6%positive => ((128 # 1) + (s IDemit_dqt_z))%Q
    | 7%positive => ((128 # 1) + (s IDemit_dqt_z))%Q
    | 8%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 9%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 10%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 11%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 12%positive => ((64 # 1) + (s IDemit_dqt_z))%Q
    | 13%positive => ((64 # 1) + (s IDemit_dqt_z))%Q
    | 14%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 15%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 16%positive => (max0(64 - (s IDemit_dqt_i)) + max0((s IDemit_dqt_z)))%Q
    | 17%positive => (max0(64 - (s IDemit_dqt_i)) + max0((s IDemit_dqt_z)))%Q
    | 18%positive => (max0(64 - (s IDemit_dqt_i)) + max0((s IDemit_dqt_z)))%Q
    | 19%positive => (max0(64 - (s IDemit_dqt_i)) + max0((s IDemit_dqt_z)))%Q
    | 20%positive => (max0(64 - (s IDemit_dqt_i)) + max0((s IDemit_dqt_z)))%Q
    | 21%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 22%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 23%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 24%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 25%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 26%positive => ((1 # 1) + (s IDemit_dqt_z)
                      + max0(63 - (s IDemit_dqt_i)))%Q
    | 27%positive => ((1 # 1) + (s IDemit_dqt_z)
                      + max0(63 - (s IDemit_dqt_i)))%Q
    | 28%positive => ((1 # 1) + (s IDemit_dqt_z)
                      + max0(63 - (s IDemit_dqt_i)))%Q
    | 29%positive => ((1 # 1) + (s IDemit_dqt_z)
                      + max0(64 - (s IDemit_dqt_i)))%Q
    | 30%positive => ((1 # 1) + (s IDemit_dqt_z)
                      + max0(64 - (s IDemit_dqt_i)))%Q
    | 31%positive => ((1 # 1) + (s IDemit_dqt_z)
                      + max0(64 - (s IDemit_dqt_i)))%Q
    | 32%positive => ((s IDemit_dqt_z) + max0(64 - (s IDemit_dqt_i)))%Q
    | 33%positive => ((64 # 1) + (s IDemit_dqt_z))%Q
    | 34%positive => ((s IDemit_dqt_z))%Q
    | 35%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 36%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 37%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 38%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 39%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 40%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 41%positive => ((129 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 42%positive => ((129 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 43%positive => ((129 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | 44%positive => ((128 # 1) - (s IDemit_dqt_i) + (s IDemit_dqt_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition emit_dqt_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDemit_dqt_i)) (63
                                                                    - (s IDemit_dqt_i)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDemit_dqt_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDemit_dqt_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (63
                                                                 - (s IDemit_dqt_i))) (F_check_ge (0) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDemit_dqt_z)) (0))) (F_max0_ge_0 ((s IDemit_dqt_z)))]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDemit_dqt_i)) (63
                                                                    - (s IDemit_dqt_i)));
                      (*-1 0*) F_max0_ge_0 (63 - (s IDemit_dqt_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDemit_dqt_z))) (F_check_ge ((s IDemit_dqt_z)) (0))]
    | 20%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDemit_dqt_z))) (F_check_ge ((s IDemit_dqt_z)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*0 1*) F_max0_pre_decrement (64 - (s IDemit_dqt_i)) (1)]
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (64 - (s IDemit_dqt_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDemit_dqt_z)) (0))) (F_max0_ge_0 ((s IDemit_dqt_z)))]
    | 33%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (64
                                                             - (s IDemit_dqt_i)) (63
                                                                    - (s IDemit_dqt_i)));
                      (*-1 0*) F_max0_ge_0 (63 - (s IDemit_dqt_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDemit_dqt_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDemit_dqt_i)) (0))) (F_max0_ge_0 ((s IDemit_dqt_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                    - (s IDemit_dqt_i)) (0))) (F_max0_ge_0 (64
                                                                    - (s IDemit_dqt_i)))]
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
    | _ => []
  end.


Theorem emit_dqt_ai_correct:
  forall s p' s', steps (g_start emit_dqt) s (g_edges emit_dqt) p' s' -> emit_dqt_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem emit_dqt_pot_correct:
  forall s p' s',
    steps (g_start emit_dqt) s (g_edges emit_dqt) p' s' ->
    (emit_dqt_pot (g_start emit_dqt) s >= emit_dqt_pot p' s')%Q.
Proof.
  check_lp emit_dqt_ai_correct emit_dqt_hints.
Qed.

