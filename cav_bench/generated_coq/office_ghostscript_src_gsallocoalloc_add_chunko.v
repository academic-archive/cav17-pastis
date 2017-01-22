Require Import pasta.Pasta.

Notation IDalloc_add_chunk_z := 1%positive.
Notation IDalloc_add_chunk__tmp := 2%positive.
Notation IDalloc_add_chunk__tmp1 := 3%positive.
Notation IDalloc_add_chunk_elt_size := 4%positive.
Notation IDalloc_add_chunk_num_elts := 5%positive.
Notation IDalloc_add_chunk_cname := 6%positive.
Notation IDalloc_add_chunk_csize := 7%positive.
Notation IDalloc_add_chunk_has_strings := 8%positive.
Notation IDalloc_add_chunk_mem := 9%positive.
Definition alloc_add_chunk : graph := {|
  g_start := 1%positive;
  g_end := 35%positive;
  g_edges := (1%positive,(AAssign IDalloc_add_chunk_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDalloc_add_chunk__tmp
             (Some (EVar IDalloc_add_chunk_csize))),3%positive)::
             (3%positive,(AAssign IDalloc_add_chunk__tmp1
             (Some (EVar IDalloc_add_chunk_has_strings))),4%positive)::
             (4%positive,(AAssign IDalloc_add_chunk_elt_size
             (Some (EVar IDalloc_add_chunk__tmp))),5%positive)::
             (5%positive,(AAssign IDalloc_add_chunk_num_elts
             (Some (ENum (1)))),6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (7%positive,ANone,15%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,33%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,33%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDalloc_add_chunk_elt_size) s) <>
             (eval (EVar IDalloc_add_chunk_elt_size) s))%Z)),26%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDalloc_add_chunk_elt_size) s) =
             (eval (EVar IDalloc_add_chunk_elt_size) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,24%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,24%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,AWeaken,35%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,35%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDalloc_add_chunk_elt_size None),
             28%positive)::
             (28%positive,(AAssign IDalloc_add_chunk_num_elts None),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDalloc_add_chunk_z (Some (EAdd (ENum (1))
             (EVar IDalloc_add_chunk_z)))),32%positive)::
             (32%positive,AWeaken,17%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::nil
|}.

Definition alloc_add_chunk_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 3%positive => (-1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 4%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 5%positive => (-1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 6%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 7%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 8%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 10%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 12%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 14%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 16%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 18%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 20%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 22%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 24%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 26%positive => (False)%Z
    | 27%positive => (False)%Z
    | 28%positive => (False)%Z
    | 29%positive => (False)%Z
    | 30%positive => (False)%Z
    | 31%positive => (False)%Z
    | 32%positive => (False)%Z
    | 33%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_z) <= 0)%Z
    | 35%positive => (1 * (s IDalloc_add_chunk_z) <= 0 /\ -1 * (s IDalloc_add_chunk_z) <= 0 /\ 1 * (s IDalloc_add_chunk_num_elts) + -1 <= 0 /\ -1 * (s IDalloc_add_chunk_num_elts) + 1 <= 0)%Z
    | _ => False
  end.

Definition alloc_add_chunk_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (0)%Q
    | 2%positive => ((s IDalloc_add_chunk_z))%Q
    | 3%positive => ((s IDalloc_add_chunk_z))%Q
    | 4%positive => ((s IDalloc_add_chunk_z))%Q
    | 5%positive => ((s IDalloc_add_chunk_z))%Q
    | 6%positive => ((s IDalloc_add_chunk_z))%Q
    | 7%positive => ((s IDalloc_add_chunk_z))%Q
    | 8%positive => ((s IDalloc_add_chunk_z))%Q
    | 9%positive => ((s IDalloc_add_chunk_z))%Q
    | 10%positive => ((s IDalloc_add_chunk_z))%Q
    | 11%positive => ((s IDalloc_add_chunk_z))%Q
    | 12%positive => ((s IDalloc_add_chunk_z))%Q
    | 13%positive => ((s IDalloc_add_chunk_z))%Q
    | 14%positive => ((s IDalloc_add_chunk_z))%Q
    | 15%positive => ((s IDalloc_add_chunk_z))%Q
    | 16%positive => ((s IDalloc_add_chunk_z))%Q
    | 17%positive => ((s IDalloc_add_chunk_z))%Q
    | 18%positive => ((s IDalloc_add_chunk_z))%Q
    | 19%positive => ((s IDalloc_add_chunk_z))%Q
    | 20%positive => ((s IDalloc_add_chunk_z))%Q
    | 21%positive => ((s IDalloc_add_chunk_z))%Q
    | 22%positive => ((s IDalloc_add_chunk_z))%Q
    | 23%positive => ((s IDalloc_add_chunk_z))%Q
    | 24%positive => ((s IDalloc_add_chunk_z))%Q
    | 25%positive => ((s IDalloc_add_chunk_z))%Q
    | 26%positive => ((s IDalloc_add_chunk_z))%Q
    | 27%positive => (0)%Q
    | 28%positive => (0)%Q
    | 29%positive => (0)%Q
    | 30%positive => (0)%Q
    | 31%positive => (0)%Q
    | 32%positive => (0)%Q
    | 33%positive => ((s IDalloc_add_chunk_z))%Q
    | 34%positive => ((s IDalloc_add_chunk_z))%Q
    | 35%positive => ((s IDalloc_add_chunk_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition alloc_add_chunk_hints (p : node) (s : state) := 
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
    | 26%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDalloc_add_chunk_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDalloc_add_chunk_z)) (0))) (F_max0_ge_0 ((s IDalloc_add_chunk_z)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDalloc_add_chunk_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDalloc_add_chunk_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDalloc_add_chunk_z)))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | _ => []
  end.


Theorem alloc_add_chunk_ai_correct:
  forall s p' s', steps (g_start alloc_add_chunk) s (g_edges alloc_add_chunk) p' s' -> alloc_add_chunk_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem alloc_add_chunk_pot_correct:
  forall s p' s',
    steps (g_start alloc_add_chunk) s (g_edges alloc_add_chunk) p' s' ->
    (alloc_add_chunk_pot (g_start alloc_add_chunk) s >= alloc_add_chunk_pot p' s')%Q.
Proof.
  check_lp alloc_add_chunk_ai_correct alloc_add_chunk_hints.
Qed.

