Require Import pasta.Pasta.

Notation IDdebug_print_string_z := 1%positive.
Notation IDdebug_print_string__tmp := 2%positive.
Notation IDdebug_print_string_i := 3%positive.
Notation IDdebug_print_string_chrs := 4%positive.
Notation IDdebug_print_string_len := 5%positive.
Definition debug_print_string : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDdebug_print_string_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_print_string_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_print_string__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDdebug_print_string__tmp
             (Some (EVar IDdebug_print_string_len))),6%positive)::
             (6%positive,(AAssign IDdebug_print_string_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_print_string_i) s) <
             (eval (EVar IDdebug_print_string__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_print_string_i) s) >=
             (eval (EVar IDdebug_print_string__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDdebug_print_string_i
             (Some (EAdd (EVar IDdebug_print_string_i) (ENum (1))))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDdebug_print_string_z
             (Some (EAdd (ENum (1)) (EVar IDdebug_print_string_z)))),
             18%positive)::(18%positive,AWeaken,9%positive)::nil
|}.

Definition debug_print_string_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdebug_print_string_z) <= 0 /\ 1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 4%positive => (-1 * (s IDdebug_print_string_i) <= 0 /\ 1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDdebug_print_string__tmp) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ 1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 6%positive => (-1 * (s IDdebug_print_string_i) <= 0 /\ 1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0)%Z
    | 7%positive => (-1 * (s IDdebug_print_string_z) <= 0 /\ 1 * (s IDdebug_print_string_z) <= 0 /\ 1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 8%positive => (-1 * (s IDdebug_print_string_i) <= 0 /\ 1 * (s IDdebug_print_string_i) <= 0 /\ 1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0)%Z
    | 9%positive => (-1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 10%positive => (-1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ 1 * (s IDdebug_print_string__tmp)+ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 11%positive => (1 * (s IDdebug_print_string__tmp)+ -1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 12%positive => (-1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) + 1 <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) <= 0)%Z
    | 14%positive => (-1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) + 1 <= 0 /\ -1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) <= 0)%Z
    | 16%positive => (-1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_i) + 1 <= 0 /\ -1 * (s IDdebug_print_string_z) <= 0)%Z
    | 17%positive => (-1 * (s IDdebug_print_string_z) <= 0 /\ -1 * (s IDdebug_print_string_i) + 1 <= 0 /\ -1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) <= 0)%Z
    | 18%positive => (-1 * (s IDdebug_print_string__tmp)+ 1 * (s IDdebug_print_string_i) <= 0 /\ -1 * (s IDdebug_print_string_i) + 1 <= 0 /\ -1 * (s IDdebug_print_string_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition debug_print_string_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDdebug_print_string_len)))%Q
    | 2%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string_len)))%Q
    | 3%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string_len)))%Q
    | 4%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string_len)))%Q
    | 5%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string_len)))%Q
    | 6%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string__tmp)))%Q
    | 7%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string__tmp)
                            - (s IDdebug_print_string_i)))%Q
    | 8%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string__tmp)
                            - (s IDdebug_print_string_i)))%Q
    | 9%positive => ((s IDdebug_print_string_z)
                     + max0((s IDdebug_print_string__tmp)
                            - (s IDdebug_print_string_i)))%Q
    | 10%positive => ((s IDdebug_print_string_z)
                      + max0((s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 11%positive => ((s IDdebug_print_string_z))%Q
    | 12%positive => ((s IDdebug_print_string_z)
                      + max0((s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 13%positive => ((1 # 1) + (s IDdebug_print_string_z)
                      + max0(-1 + (s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 14%positive => ((1 # 1) + (s IDdebug_print_string_z)
                      + max0(-1 + (s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 15%positive => ((1 # 1) + (s IDdebug_print_string_z)
                      + max0((s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 16%positive => ((1 # 1) + (s IDdebug_print_string_z)
                      + max0((s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 17%positive => ((1 # 1) + (s IDdebug_print_string_z)
                      + max0((s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | 18%positive => ((s IDdebug_print_string_z)
                      + max0((s IDdebug_print_string__tmp)
                             - (s IDdebug_print_string_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition debug_print_string_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDdebug_print_string__tmp)
                                                             - (s IDdebug_print_string_i)) (-1
                                                                    + (s IDdebug_print_string__tmp)
                                                                    - (s IDdebug_print_string_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDdebug_print_string__tmp)
                                            - (s IDdebug_print_string_i))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement ((s IDdebug_print_string__tmp)
                                                     - (s IDdebug_print_string_i)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | _ => []
  end.


Theorem debug_print_string_ai_correct:
  forall s p' s', steps (g_start debug_print_string) s (g_edges debug_print_string) p' s' -> debug_print_string_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem debug_print_string_pot_correct:
  forall s p' s',
    steps (g_start debug_print_string) s (g_edges debug_print_string) p' s' ->
    (debug_print_string_pot (g_start debug_print_string) s >= debug_print_string_pot p' s')%Q.
Proof.
  check_lp debug_print_string_ai_correct debug_print_string_hints.
Qed.

