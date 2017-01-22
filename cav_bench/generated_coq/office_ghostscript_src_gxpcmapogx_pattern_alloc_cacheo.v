Require Import pasta.Pasta.

Notation IDgx_pattern_alloc_cache_z := 1%positive.
Notation IDgx_pattern_alloc_cache__tmp := 2%positive.
Notation IDgx_pattern_alloc_cache__tmp1 := 3%positive.
Notation IDgx_pattern_alloc_cache_i := 4%positive.
Notation IDgx_pattern_alloc_cache_max_bits := 5%positive.
Notation IDgx_pattern_alloc_cache_mem := 6%positive.
Notation IDgx_pattern_alloc_cache_num_tiles := 7%positive.
Definition gx_pattern_alloc_cache : graph := {|
  g_start := 1%positive;
  g_end := 27%positive;
  g_edges := (1%positive,(AAssign IDgx_pattern_alloc_cache_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDgx_pattern_alloc_cache_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDgx_pattern_alloc_cache__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDgx_pattern_alloc_cache__tmp
             (Some (EVar IDgx_pattern_alloc_cache_num_tiles))),6%positive)::
             (6%positive,(AAssign IDgx_pattern_alloc_cache__tmp1
             (Some (EVar IDgx_pattern_alloc_cache_max_bits))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,25%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,25%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDgx_pattern_alloc_cache_i
             (Some (ENum (0)))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDgx_pattern_alloc_cache_i) s) <
             (eval (EVar IDgx_pattern_alloc_cache__tmp) s))%Z)),18%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDgx_pattern_alloc_cache_i) s) >=
             (eval (EVar IDgx_pattern_alloc_cache__tmp) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,27%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDgx_pattern_alloc_cache_i
             (Some (EAdd (EVar IDgx_pattern_alloc_cache_i) (ENum (1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDgx_pattern_alloc_cache_z
             (Some (EAdd (ENum (1)) (EVar IDgx_pattern_alloc_cache_z)))),
             24%positive)::(24%positive,AWeaken,14%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,27%positive)::nil
|}.

Definition gx_pattern_alloc_cache_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_pattern_alloc_cache__tmp) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 8%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 10%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 11%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 12%positive => (1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 13%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 14%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 15%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache__tmp)+ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 16%positive => (1 * (s IDgx_pattern_alloc_cache__tmp)+ -1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 17%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache__tmp)+ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 18%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 20%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0 /\ -1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 22%positive => (-1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 23%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0 /\ -1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 24%positive => (-1 * (s IDgx_pattern_alloc_cache__tmp)+ 1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) + 1 <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | 26%positive => (-1 * (s IDgx_pattern_alloc_cache_i) <= 0 /\ 1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_z) <= 0)%Z
    | 27%positive => (-1 * (s IDgx_pattern_alloc_cache_z) <= 0 /\ -1 * (s IDgx_pattern_alloc_cache_i) <= 0)%Z
    | _ => False
  end.

Definition gx_pattern_alloc_cache_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgx_pattern_alloc_cache_num_tiles)))%Q
    | 2%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache_num_tiles)))%Q
    | 3%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache_num_tiles)))%Q
    | 4%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache_num_tiles)))%Q
    | 5%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache_num_tiles)))%Q
    | 6%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 7%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 8%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 9%positive => ((s IDgx_pattern_alloc_cache_z)
                     + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 10%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 11%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 12%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 13%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 14%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 15%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 16%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 17%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 18%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 19%positive => ((1 # 1) + (s IDgx_pattern_alloc_cache_z)
                      + max0(-1 + (s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 20%positive => ((1 # 1) + (s IDgx_pattern_alloc_cache_z)
                      + max0(-1 + (s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 21%positive => ((1 # 1) + (s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 22%positive => ((1 # 1) + (s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 23%positive => ((1 # 1) + (s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 24%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)
                             - (s IDgx_pattern_alloc_cache_i)))%Q
    | 25%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 26%positive => ((s IDgx_pattern_alloc_cache_z)
                      + max0((s IDgx_pattern_alloc_cache__tmp)))%Q
    | 27%positive => ((s IDgx_pattern_alloc_cache_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_pattern_alloc_cache_hints (p : node) (s : state) := 
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
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgx_pattern_alloc_cache__tmp)
                                                             - (s IDgx_pattern_alloc_cache_i)) (-1
                                                                    + (s IDgx_pattern_alloc_cache__tmp)
                                                                    - (s IDgx_pattern_alloc_cache_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgx_pattern_alloc_cache__tmp)
                                            - (s IDgx_pattern_alloc_cache_i))]
    | 18%positive => [(*0 1*) F_max0_pre_decrement ((s IDgx_pattern_alloc_cache__tmp)
                                                    - (s IDgx_pattern_alloc_cache_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_ge_0 ((s IDgx_pattern_alloc_cache__tmp))]
    | 27%positive => []
    | _ => []
  end.


Theorem gx_pattern_alloc_cache_ai_correct:
  forall s p' s', steps (g_start gx_pattern_alloc_cache) s (g_edges gx_pattern_alloc_cache) p' s' -> gx_pattern_alloc_cache_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_pattern_alloc_cache_pot_correct:
  forall s p' s',
    steps (g_start gx_pattern_alloc_cache) s (g_edges gx_pattern_alloc_cache) p' s' ->
    (gx_pattern_alloc_cache_pot (g_start gx_pattern_alloc_cache) s >= gx_pattern_alloc_cache_pot p' s')%Q.
Proof.
  check_lp gx_pattern_alloc_cache_ai_correct gx_pattern_alloc_cache_hints.
Qed.

