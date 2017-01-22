Require Import pasta.Pasta.

Notation IDcie_prepare_caches_z := 1%positive.
Notation IDcie_prepare_caches__tmp := 2%positive.
Notation IDcie_prepare_caches__tmp1 := 3%positive.
Notation IDcie_prepare_caches_code := 4%positive.
Notation IDcie_prepare_caches_i := 5%positive.
Notation IDcie_prepare_caches_cname := 6%positive.
Notation IDcie_prepare_caches_container := 7%positive.
Notation IDcie_prepare_caches_count := 8%positive.
Notation IDcie_prepare_caches_domains := 9%positive.
Notation IDcie_prepare_caches_pgs := 10%positive.
Notation IDcie_prepare_caches_ppc := 11%positive.
Notation IDcie_prepare_caches_procs := 12%positive.
Definition cie_prepare_caches : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDcie_prepare_caches_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcie_prepare_caches__tmp
             (Some (EVar IDcie_prepare_caches_count))),3%positive)::
             (3%positive,(AAssign IDcie_prepare_caches_code
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDcie_prepare_caches_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcie_prepare_caches_i) s) <
             (eval (EVar IDcie_prepare_caches__tmp) s))%Z)),12%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDcie_prepare_caches_i) s) >=
             (eval (EVar IDcie_prepare_caches__tmp) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDcie_prepare_caches__tmp1
             (Some (EVar IDcie_prepare_caches_code))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,25%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDcie_prepare_caches_code None),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,ANone,22%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDcie_prepare_caches_i
             (Some (EAdd (EVar IDcie_prepare_caches_i) (ENum (1))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDcie_prepare_caches_z
             (Some (EAdd (ENum (1)) (EVar IDcie_prepare_caches_z)))),
             21%positive)::(21%positive,AWeaken,7%positive)::
             (22%positive,(AAssign IDcie_prepare_caches__tmp1
             (Some (EVar IDcie_prepare_caches_code))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::nil
|}.

Definition cie_prepare_caches_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches_z) <= 0)%Z
    | 4%positive => (1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches_code) <= 0 /\ -1 * (s IDcie_prepare_caches_code) <= 0)%Z
    | 5%positive => (-1 * (s IDcie_prepare_caches_code) <= 0 /\ 1 * (s IDcie_prepare_caches_code) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 6%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ 1 * (s IDcie_prepare_caches_i) <= 0 /\ 1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches_code) <= 0 /\ -1 * (s IDcie_prepare_caches_code) <= 0)%Z
    | 7%positive => (-1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 8%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches__tmp)+ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 9%positive => (1 * (s IDcie_prepare_caches__tmp)+ -1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 10%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ 1 * (s IDcie_prepare_caches__tmp)+ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 11%positive => (1 * (s IDcie_prepare_caches__tmp)+ -1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 12%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 14%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 16%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 18%positive => (-1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDcie_prepare_caches_i) + 1 <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0)%Z
    | 20%positive => (-1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcie_prepare_caches_i) + 1 <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | 24%positive => (-1 * (s IDcie_prepare_caches_i) <= 0 /\ -1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches__tmp)+ 1 * (s IDcie_prepare_caches_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDcie_prepare_caches_z) <= 0 /\ -1 * (s IDcie_prepare_caches_i) <= 0)%Z
    | _ => False
  end.

Definition cie_prepare_caches_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcie_prepare_caches_count)))%Q
    | 2%positive => (max0((s IDcie_prepare_caches_count))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 3%positive => (max0((s IDcie_prepare_caches__tmp))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 4%positive => (max0((s IDcie_prepare_caches__tmp))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 5%positive => (max0((s IDcie_prepare_caches__tmp)
                          - (s IDcie_prepare_caches_i))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 6%positive => (max0((s IDcie_prepare_caches__tmp)
                          - (s IDcie_prepare_caches_i))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 7%positive => (max0((s IDcie_prepare_caches__tmp)
                          - (s IDcie_prepare_caches_i))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 8%positive => (max0((s IDcie_prepare_caches__tmp)
                          - (s IDcie_prepare_caches_i))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 9%positive => (max0((s IDcie_prepare_caches__tmp)
                          - (s IDcie_prepare_caches_i))
                     + max0((s IDcie_prepare_caches_z)))%Q
    | 10%positive => (max0((s IDcie_prepare_caches__tmp)
                           - (s IDcie_prepare_caches_i))
                      + max0((s IDcie_prepare_caches_z)))%Q
    | 11%positive => (max0((s IDcie_prepare_caches__tmp)
                           - (s IDcie_prepare_caches_i))
                      + max0((s IDcie_prepare_caches_z)))%Q
    | 12%positive => (max0((s IDcie_prepare_caches__tmp)
                           - (s IDcie_prepare_caches_i))
                      + max0((s IDcie_prepare_caches_z)))%Q
    | 13%positive => (max0((s IDcie_prepare_caches__tmp)
                           - (s IDcie_prepare_caches_i))
                      + max0((s IDcie_prepare_caches_z)))%Q
    | 14%positive => (max0((s IDcie_prepare_caches__tmp)
                           - (s IDcie_prepare_caches_i))
                      + max0((s IDcie_prepare_caches_z)))%Q
    | 15%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0(-1 + (s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 16%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0(-1 + (s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 17%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0(-1 + (s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 18%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0((s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 19%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0((s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 20%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0((s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 21%positive => ((s IDcie_prepare_caches_z)
                      + max0((s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 22%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0(-1 + (s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 23%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0(-1 + (s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 24%positive => ((1 # 1) + (s IDcie_prepare_caches_z)
                      + max0(-1 + (s IDcie_prepare_caches__tmp)
                             - (s IDcie_prepare_caches_i)))%Q
    | 25%positive => ((s IDcie_prepare_caches_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cie_prepare_caches_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcie_prepare_caches__tmp)
                                                             - (s IDcie_prepare_caches_i)) (-1
                                                                    + (s IDcie_prepare_caches__tmp)
                                                                    - (s IDcie_prepare_caches_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcie_prepare_caches__tmp)
                                            - (s IDcie_prepare_caches_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcie_prepare_caches_z))) (F_check_ge ((s IDcie_prepare_caches_z)) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*0 1*) F_max0_pre_decrement ((s IDcie_prepare_caches__tmp)
                                                    - (s IDcie_prepare_caches_i)) (1);
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcie_prepare_caches_z))) (F_check_ge ((s IDcie_prepare_caches_z)) (0))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcie_prepare_caches_z)) (0))) (F_max0_ge_0 ((s IDcie_prepare_caches_z)))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcie_prepare_caches__tmp)
                                            - (s IDcie_prepare_caches_i))]
    | 25%positive => []
    | _ => []
  end.


Theorem cie_prepare_caches_ai_correct:
  forall s p' s', steps (g_start cie_prepare_caches) s (g_edges cie_prepare_caches) p' s' -> cie_prepare_caches_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cie_prepare_caches_pot_correct:
  forall s p' s',
    steps (g_start cie_prepare_caches) s (g_edges cie_prepare_caches) p' s' ->
    (cie_prepare_caches_pot (g_start cie_prepare_caches) s >= cie_prepare_caches_pot p' s')%Q.
Proof.
  check_lp cie_prepare_caches_ai_correct cie_prepare_caches_hints.
Qed.

