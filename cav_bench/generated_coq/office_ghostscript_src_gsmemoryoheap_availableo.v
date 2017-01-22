Require Import pasta.Pasta.

Notation IDheap_available_z := 1%positive.
Notation IDheap_available_avail := 2%positive.
Notation IDheap_available_n := 3%positive.
Definition heap_available : graph := {|
  g_start := 1%positive;
  g_end := 26%positive;
  g_edges := (1%positive,(AAssign IDheap_available_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDheap_available_n)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDheap_available_avail (Some (ENum (0)))),
             5%positive)::
             (5%positive,(AAssign IDheap_available_n (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDheap_available_n)
             s) < (eval (ENum (20)) s))%Z)),10%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDheap_available_n)
             s) >= (eval (ENum (20)) s))%Z)),9%positive)::
             (9%positive,AWeaken,22%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,21%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDheap_available_avail None),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDheap_available_n
             (Some (EAdd (EVar IDheap_available_n) (ENum (1))))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDheap_available_z (Some (EAdd (ENum (1))
             (EVar IDheap_available_z)))),20%positive)::
             (20%positive,AWeaken,8%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDheap_available_n)
             s) <> (eval (ENum (0)) s))%Z)),27%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDheap_available_n)
             s) = (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDheap_available_n
             (Some (EAdd (EVar IDheap_available_n) (ENum (-1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDheap_available_z (Some (EAdd (ENum (1))
             (EVar IDheap_available_z)))),32%positive)::
             (32%positive,AWeaken,24%positive)::nil
|}.

Definition heap_available_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_z) <= 0)%Z
    | 3%positive => (-1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 4%positive => (-1 * (s IDheap_available_n) <= 0 /\ 1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_z) <= 0)%Z
    | 5%positive => (-1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0 /\ 1 * (s IDheap_available_avail) <= 0 /\ -1 * (s IDheap_available_avail) <= 0)%Z
    | 6%positive => (-1 * (s IDheap_available_avail) <= 0 /\ 1 * (s IDheap_available_avail) <= 0 /\ 1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 7%positive => (-1 * (s IDheap_available_n) <= 0 /\ 1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_avail) <= 0 /\ -1 * (s IDheap_available_avail) <= 0)%Z
    | 8%positive => (-1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0 /\ 1 * (s IDheap_available_n) + -20 <= 0)%Z
    | 9%positive => (1 * (s IDheap_available_n) + -20 <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) + 20 <= 0)%Z
    | 10%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0)%Z
    | 11%positive => (1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 12%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0)%Z
    | 13%positive => (1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 14%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0)%Z
    | 15%positive => (1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 16%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0)%Z
    | 17%positive => (-1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) + 1 <= 0 /\ 1 * (s IDheap_available_n) + -20 <= 0)%Z
    | 18%positive => (1 * (s IDheap_available_n) + -20 <= 0 /\ -1 * (s IDheap_available_n) + 1 <= 0 /\ -1 * (s IDheap_available_z) <= 0)%Z
    | 19%positive => (-1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) + 1 <= 0 /\ 1 * (s IDheap_available_n) + -20 <= 0)%Z
    | 20%positive => (1 * (s IDheap_available_n) + -20 <= 0 /\ -1 * (s IDheap_available_n) + 1 <= 0 /\ -1 * (s IDheap_available_z) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0)%Z
    | 22%positive => (1 * (s IDheap_available_n) + -20 <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 23%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -20 <= 0)%Z
    | 24%positive => (1 * (s IDheap_available_n) + -20 <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 25%positive => (-1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) <= 0)%Z
    | 26%positive => (1 * (s IDheap_available_n) <= 0 /\ -1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 27%positive => (-1 * (s IDheap_available_z) <= 0 /\ -1 * (s IDheap_available_n) + 1 <= 0 /\ 1 * (s IDheap_available_n) + -20 <= 0)%Z
    | 28%positive => (1 * (s IDheap_available_n) + -20 <= 0 /\ -1 * (s IDheap_available_n) + 1 <= 0 /\ -1 * (s IDheap_available_z) <= 0)%Z
    | 29%positive => (-1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 30%positive => (-1 * (s IDheap_available_n) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_z) <= 0)%Z
    | 31%positive => (-1 * (s IDheap_available_z) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_n) <= 0)%Z
    | 32%positive => (-1 * (s IDheap_available_n) <= 0 /\ 1 * (s IDheap_available_n) + -19 <= 0 /\ -1 * (s IDheap_available_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition heap_available_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((40 # 1))%Q
    | 2%positive => ((40 # 1) + max0((s IDheap_available_z)))%Q
    | 3%positive => ((40 # 1) + max0((s IDheap_available_z)))%Q
    | 4%positive => ((40 # 1) + max0((s IDheap_available_z)))%Q
    | 5%positive => ((40 # 1) + max0((s IDheap_available_z)))%Q
    | 6%positive => ((40 # 1) - (s IDheap_available_n)
                     + max0((s IDheap_available_z)))%Q
    | 7%positive => ((40 # 1) - (s IDheap_available_n)
                     + max0((s IDheap_available_z)))%Q
    | 8%positive => ((40 # 1) - (s IDheap_available_n)
                     + max0((s IDheap_available_z)))%Q
    | 9%positive => ((40 # 1) - (s IDheap_available_n)
                     + max0((s IDheap_available_z)))%Q
    | 10%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 11%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 12%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 13%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 14%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 15%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 16%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 17%positive => ((41 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 18%positive => ((41 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 19%positive => ((41 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 20%positive => ((41 # 1) - (s IDheap_available_n)
                      + max0(-1 + (s IDheap_available_z)))%Q
    | 21%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 22%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 23%positive => ((40 # 1) - (s IDheap_available_n)
                      + max0((s IDheap_available_z)))%Q
    | 24%positive => ((s IDheap_available_n) + (s IDheap_available_z))%Q
    | 25%positive => ((s IDheap_available_n) + (s IDheap_available_z))%Q
    | 26%positive => ((s IDheap_available_z))%Q
    | 27%positive => ((s IDheap_available_n) + (s IDheap_available_z))%Q
    | 28%positive => ((s IDheap_available_n) + (s IDheap_available_z))%Q
    | 29%positive => ((1 # 1) + (s IDheap_available_n)
                      + (s IDheap_available_z))%Q
    | 30%positive => ((1 # 1) + (s IDheap_available_n)
                      + (s IDheap_available_z))%Q
    | 31%positive => ((1 # 1) + (s IDheap_available_n)
                      + (s IDheap_available_z))%Q
    | 32%positive => ((s IDheap_available_n) + (s IDheap_available_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition heap_available_hints (p : node) (s : state) := 
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
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDheap_available_z)) (0))) (F_max0_ge_0 ((s IDheap_available_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   (s IDheap_available_z))) (F_check_ge (-1
                                                                    + (s IDheap_available_z)) (0))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-2 0*) F_max0_monotonic (F_check_ge (20
                                                             - (s IDheap_available_n)) (19
                                                                    - (s IDheap_available_n)));
                      (*-2 0*) F_max0_ge_0 (19 - (s IDheap_available_n));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDheap_available_z))) (F_check_ge ((s IDheap_available_z)) (0));
                      (*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - (s IDheap_available_n)) (0))) (F_max0_ge_0 (20
                                                                    - (s IDheap_available_n)))]
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDheap_available_n))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDheap_available_n)) (0))) (F_max0_ge_0 ((s IDheap_available_n)))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | _ => []
  end.


Theorem heap_available_ai_correct:
  forall s p' s', steps (g_start heap_available) s (g_edges heap_available) p' s' -> heap_available_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem heap_available_pot_correct:
  forall s p' s',
    steps (g_start heap_available) s (g_edges heap_available) p' s' ->
    (heap_available_pot (g_start heap_available) s >= heap_available_pot p' s')%Q.
Proof.
  check_lp heap_available_ai_correct heap_available_hints.
Qed.

