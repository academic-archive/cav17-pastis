Require Import pasta.Pasta.

Notation IDread_colormap_z := 1%positive.
Notation IDread_colormap__tmp := 2%positive.
Notation IDread_colormap__tmp1 := 3%positive.
Notation IDread_colormap_i := 4%positive.
Notation IDread_colormap_cmaplen := 5%positive.
Notation IDread_colormap_mapentrysize := 6%positive.
Notation IDread_colormap_sinfo := 7%positive.
Definition read_colormap : graph := {|
  g_start := 1%positive;
  g_end := 36%positive;
  g_edges := (1%positive,(AAssign IDread_colormap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDread_colormap__tmp
             (Some (EVar IDread_colormap_cmaplen))),3%positive)::
             (3%positive,(AAssign IDread_colormap__tmp1
             (Some (EVar IDread_colormap_mapentrysize))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,34%positive)::(5%positive,ANone,20%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDread_colormap_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDread_colormap_i)
             s) < (eval (EVar IDread_colormap__tmp) s))%Z)),13%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDread_colormap_i)
             s) >= (eval (EVar IDread_colormap__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,36%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDread_colormap_i
             (Some (EAdd (EVar IDread_colormap_i) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDread_colormap_z (Some (EAdd (ENum (1))
             (EVar IDread_colormap_z)))),19%positive)::
             (19%positive,AWeaken,9%positive)::
             (20%positive,(AAssign IDread_colormap_i (Some (ENum (0)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDread_colormap_i)
             s) < (eval (EVar IDread_colormap__tmp) s))%Z)),27%positive)::
             (23%positive,(AGuard (fun s => ((eval (EVar IDread_colormap_i)
             s) >= (eval (EVar IDread_colormap__tmp) s))%Z)),24%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,36%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDread_colormap_i
             (Some (EAdd (EVar IDread_colormap_i) (ENum (1))))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,(AAssign IDread_colormap_z (Some (EAdd (ENum (1))
             (EVar IDread_colormap_z)))),33%positive)::
             (33%positive,AWeaken,23%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::nil
|}.

Definition read_colormap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0)%Z
    | 4%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0)%Z
    | 6%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 7%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 8%positive => (-1 * (s IDread_colormap_i) <= 0 /\ 1 * (s IDread_colormap_i) <= 0 /\ 1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 9%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 10%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap__tmp)+ -1 * (s IDread_colormap_i) <= 0)%Z
    | 11%positive => (1 * (s IDread_colormap__tmp)+ -1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 12%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap__tmp)+ -1 * (s IDread_colormap_i) <= 0)%Z
    | 13%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 15%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0)%Z
    | 17%positive => (-1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 18%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0)%Z
    | 19%positive => (-1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 21%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 22%positive => (-1 * (s IDread_colormap_i) <= 0 /\ 1 * (s IDread_colormap_i) <= 0 /\ 1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 23%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 24%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap__tmp)+ -1 * (s IDread_colormap_i) <= 0)%Z
    | 25%positive => (1 * (s IDread_colormap__tmp)+ -1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 26%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap__tmp)+ -1 * (s IDread_colormap_i) <= 0)%Z
    | 27%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 29%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0)%Z
    | 31%positive => (-1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 32%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0)%Z
    | 33%positive => (-1 * (s IDread_colormap__tmp)+ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 35%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0)%Z
    | 36%positive => (-1 * (s IDread_colormap_z) <= 0)%Z
    | _ => False
  end.

Definition read_colormap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDread_colormap_cmaplen)))%Q
    | 2%positive => (max0((s IDread_colormap_cmaplen))
                     + max0((s IDread_colormap_z)))%Q
    | 3%positive => (max0((s IDread_colormap__tmp))
                     + max0((s IDread_colormap_z)))%Q
    | 4%positive => (max0((s IDread_colormap__tmp))
                     + max0((s IDread_colormap_z)))%Q
    | 5%positive => (max0((s IDread_colormap__tmp))
                     + max0((s IDread_colormap_z)))%Q
    | 6%positive => (max0((s IDread_colormap__tmp))
                     + max0((s IDread_colormap_z)))%Q
    | 7%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                     + max0((s IDread_colormap_z)))%Q
    | 8%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                     + max0((s IDread_colormap_z)))%Q
    | 9%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                     + max0((s IDread_colormap_z)))%Q
    | 10%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 11%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 12%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 13%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 14%positive => ((1 # 1)
                      + max0(-1 + (s IDread_colormap__tmp)
                             - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 15%positive => ((1 # 1)
                      + max0(-1 + (s IDread_colormap__tmp)
                             - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 16%positive => ((1 # 1)
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 17%positive => ((1 # 1)
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 18%positive => ((1 # 1)
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 19%positive => ((1 # 1) + max0(-1 + (s IDread_colormap_z))
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i)))%Q
    | 20%positive => (max0((s IDread_colormap__tmp))
                      + max0((s IDread_colormap_z)))%Q
    | 21%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 22%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 23%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 24%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 25%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 26%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 27%positive => (max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 28%positive => ((1 # 1)
                      + max0(-1 + (s IDread_colormap__tmp)
                             - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 29%positive => ((1 # 1)
                      + max0(-1 + (s IDread_colormap__tmp)
                             - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 30%positive => ((1 # 1)
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 31%positive => ((1 # 1)
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 32%positive => ((1 # 1)
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i))
                      + max0((s IDread_colormap_z)))%Q
    | 33%positive => ((1 # 1) + max0(-1 + (s IDread_colormap_z))
                      + max0((s IDread_colormap__tmp) - (s IDread_colormap_i)))%Q
    | 34%positive => (max0((s IDread_colormap__tmp))
                      + max0((s IDread_colormap_z)))%Q
    | 35%positive => (max0((s IDread_colormap__tmp))
                      + max0((s IDread_colormap_z)))%Q
    | 36%positive => ((s IDread_colormap_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_colormap_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDread_colormap__tmp)
                                                             - (s IDread_colormap_i)) (-1
                                                                    + (s IDread_colormap__tmp)
                                                                    - (s IDread_colormap_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDread_colormap__tmp)
                                            - (s IDread_colormap_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_colormap_z))) (F_check_ge ((s IDread_colormap_z)) (0))]
    | 13%positive => [(*-1 0*) F_max0_pre_decrement ((s IDread_colormap__tmp)
                                                     - (s IDread_colormap_i)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDread_colormap_z)) (0))) (F_max0_ge_0 ((s IDread_colormap_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDread_colormap_z))) (F_check_ge (-1
                                                                    + (s IDread_colormap_z)) (0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDread_colormap__tmp)
                                                             - (s IDread_colormap_i)) (-1
                                                                    + (s IDread_colormap__tmp)
                                                                    - (s IDread_colormap_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDread_colormap__tmp)
                                            - (s IDread_colormap_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_colormap_z))) (F_check_ge ((s IDread_colormap_z)) (0))]
    | 27%positive => [(*-1 0*) F_max0_pre_decrement ((s IDread_colormap__tmp)
                                                     - (s IDread_colormap_i)) (1)]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDread_colormap_z)) (0))) (F_max0_ge_0 ((s IDread_colormap_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDread_colormap_z))) (F_check_ge (-1
                                                                    + (s IDread_colormap_z)) (0))]
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_ge_0 ((s IDread_colormap__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_colormap_z))) (F_check_ge ((s IDread_colormap_z)) (0))]
    | 36%positive => []
    | _ => []
  end.


Theorem read_colormap_ai_correct:
  forall s p' s', steps (g_start read_colormap) s (g_edges read_colormap) p' s' -> read_colormap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_colormap_pot_correct:
  forall s p' s',
    steps (g_start read_colormap) s (g_edges read_colormap) p' s' ->
    (read_colormap_pot (g_start read_colormap) s >= read_colormap_pot p' s')%Q.
Proof.
  check_lp read_colormap_ai_correct read_colormap_hints.
Qed.

