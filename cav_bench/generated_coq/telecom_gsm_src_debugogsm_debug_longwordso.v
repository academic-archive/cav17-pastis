Require Import pasta.Pasta.

Notation IDgsm_debug_longwords_z := 1%positive.
Notation IDgsm_debug_longwords__tmp := 2%positive.
Notation IDgsm_debug_longwords__tmp1 := 3%positive.
Notation IDgsm_debug_longwords_nprinted := 4%positive.
Notation IDgsm_debug_longwords_from := 5%positive.
Notation IDgsm_debug_longwords_name := 6%positive.
Notation IDgsm_debug_longwords_ptr := 7%positive.
Notation IDgsm_debug_longwords_to := 8%positive.
Definition gsm_debug_longwords : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDgsm_debug_longwords_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgsm_debug_longwords__tmp
             (Some (EVar IDgsm_debug_longwords_from))),3%positive)::
             (3%positive,(AAssign IDgsm_debug_longwords__tmp1
             (Some (EVar IDgsm_debug_longwords_to))),4%positive)::
             (4%positive,(AAssign IDgsm_debug_longwords_nprinted
             (Some (ENum (0)))),5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgsm_debug_longwords__tmp) s) <=
             (eval (EVar IDgsm_debug_longwords__tmp1) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgsm_debug_longwords__tmp) s) >
             (eval (EVar IDgsm_debug_longwords__tmp1) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDgsm_debug_longwords__tmp
             (Some (EAdd (EVar IDgsm_debug_longwords__tmp) (ENum (1))))),
             12%positive)::
             (12%positive,(AAssign IDgsm_debug_longwords_nprinted
             (Some (EAdd (EVar IDgsm_debug_longwords_nprinted) (ENum (1))))),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDgsm_debug_longwords_nprinted) s) >=
             (eval (ENum (7)) s))%Z)),16%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDgsm_debug_longwords_nprinted) s) <
             (eval (ENum (7)) s))%Z)),15%positive)::
             (15%positive,AWeaken,24%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDgsm_debug_longwords_nprinted
             (Some (ENum (0)))),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDgsm_debug_longwords__tmp) s) <
             (eval (EVar IDgsm_debug_longwords__tmp1) s))%Z)),21%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDgsm_debug_longwords__tmp) s) >=
             (eval (EVar IDgsm_debug_longwords__tmp1) s))%Z)),20%positive)::
             (20%positive,AWeaken,23%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDgsm_debug_longwords_z
             (Some (EAdd (ENum (1)) (EVar IDgsm_debug_longwords_z)))),
             27%positive)::(27%positive,AWeaken,7%positive)::nil
|}.

Definition gsm_debug_longwords_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_z) <= 0)%Z
    | 4%positive => (1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0)%Z
    | 6%positive => (-1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0)%Z
    | 8%positive => (-1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords__tmp)+ 1 * (s IDgsm_debug_longwords__tmp1) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDgsm_debug_longwords__tmp)+ 1 * (s IDgsm_debug_longwords__tmp1) + 1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0)%Z
    | 10%positive => (-1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) <= 0)%Z
    | 11%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0)%Z
    | 12%positive => (-1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0)%Z
    | 13%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) + 1 <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -7 <= 0)%Z
    | 14%positive => (1 * (s IDgsm_debug_longwords_nprinted) + -7 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) + 1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0)%Z
    | 15%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) + 1 <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0)%Z
    | 16%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -7 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) + 7 <= 0)%Z
    | 17%positive => (-1 * (s IDgsm_debug_longwords_nprinted) + 7 <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -7 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0)%Z
    | 18%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0)%Z
    | 19%positive => (-1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0)%Z
    | 20%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords__tmp)+ 1 * (s IDgsm_debug_longwords__tmp1) <= 0)%Z
    | 21%positive => (-1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + 1 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0)%Z
    | 23%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0)%Z
    | 24%positive => (1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0)%Z
    | 25%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0)%Z
    | 26%positive => (1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ -1 * (s IDgsm_debug_longwords_z) <= 0 /\ 1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0)%Z
    | 27%positive => (1 * (s IDgsm_debug_longwords__tmp)+ -1 * (s IDgsm_debug_longwords__tmp1) + -1 <= 0 /\ -1 * (s IDgsm_debug_longwords_nprinted) <= 0 /\ 1 * (s IDgsm_debug_longwords_nprinted) + -6 <= 0 /\ -1 * (s IDgsm_debug_longwords_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gsm_debug_longwords_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 - (s IDgsm_debug_longwords_from)
                          + (s IDgsm_debug_longwords_to)))%Q
    | 2%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords_from)
                            + (s IDgsm_debug_longwords_to)))%Q
    | 3%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords__tmp)
                            + (s IDgsm_debug_longwords_to)))%Q
    | 4%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords__tmp)
                            + (s IDgsm_debug_longwords__tmp1)))%Q
    | 5%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords__tmp)
                            + (s IDgsm_debug_longwords__tmp1)))%Q
    | 6%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords__tmp)
                            + (s IDgsm_debug_longwords__tmp1)))%Q
    | 7%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords__tmp)
                            + (s IDgsm_debug_longwords__tmp1)))%Q
    | 8%positive => ((s IDgsm_debug_longwords_z)
                     + max0(1 - (s IDgsm_debug_longwords__tmp)
                            + (s IDgsm_debug_longwords__tmp1)))%Q
    | 9%positive => ((s IDgsm_debug_longwords_z))%Q
    | 10%positive => ((s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 11%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(-(s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 12%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 13%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 14%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 15%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 16%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 17%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 18%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 19%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 20%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 21%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 22%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 23%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 24%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 25%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 26%positive => ((1 # 1) + (s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | 27%positive => ((s IDgsm_debug_longwords_z)
                      + max0(1 - (s IDgsm_debug_longwords__tmp)
                             + (s IDgsm_debug_longwords__tmp1)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gsm_debug_longwords_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            - (s IDgsm_debug_longwords__tmp)
                                                            + (s IDgsm_debug_longwords__tmp1)) (-
                                                                    (s IDgsm_debug_longwords__tmp)
                                                                    + (s IDgsm_debug_longwords__tmp1)));
                     (*-1 0*) F_max0_ge_0 (-(s IDgsm_debug_longwords__tmp)
                                           + (s IDgsm_debug_longwords__tmp1))]
    | 9%positive => []
    | 10%positive => [(*0 1*) F_max0_pre_decrement (1
                                                    - (s IDgsm_debug_longwords__tmp)
                                                    + (s IDgsm_debug_longwords__tmp1)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDgsm_debug_longwords_nprinted))) (F_check_ge (-1
                                                                    + (s IDgsm_debug_longwords_nprinted)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDgsm_debug_longwords_nprinted)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDgsm_debug_longwords_nprinted)))]
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
    | _ => []
  end.


Theorem gsm_debug_longwords_ai_correct:
  forall s p' s', steps (g_start gsm_debug_longwords) s (g_edges gsm_debug_longwords) p' s' -> gsm_debug_longwords_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gsm_debug_longwords_pot_correct:
  forall s p' s',
    steps (g_start gsm_debug_longwords) s (g_edges gsm_debug_longwords) p' s' ->
    (gsm_debug_longwords_pot (g_start gsm_debug_longwords) s >= gsm_debug_longwords_pot p' s')%Q.
Proof.
  check_lp gsm_debug_longwords_ai_correct gsm_debug_longwords_hints.
Qed.

