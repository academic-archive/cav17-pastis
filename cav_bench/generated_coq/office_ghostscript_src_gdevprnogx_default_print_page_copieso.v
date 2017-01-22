Require Import pasta.Pasta.

Notation IDgx_default_print_page_copies_z := 1%positive.
Notation IDgx_default_print_page_copies__tmp := 2%positive.
Notation IDgx_default_print_page_copies_code := 3%positive.
Notation IDgx_default_print_page_copies_i := 4%positive.
Notation IDgx_default_print_page_copies_num_copies := 5%positive.
Notation IDgx_default_print_page_copies_pdev := 6%positive.
Notation IDgx_default_print_page_copies_prn_stream := 7%positive.
Definition gx_default_print_page_copies : graph := {|
  g_start := 1%positive;
  g_end := 15%positive;
  g_edges := (1%positive,(AAssign IDgx_default_print_page_copies_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDgx_default_print_page_copies__tmp
             (Some (EVar IDgx_default_print_page_copies_num_copies))),
             3%positive)::
             (3%positive,(AAssign IDgx_default_print_page_copies_i
             (Some (EVar IDgx_default_print_page_copies__tmp))),4%positive)::
             (4%positive,(AAssign IDgx_default_print_page_copies_code
             (Some (ENum (0)))),5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgx_default_print_page_copies_code)
             s) >= (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDgx_default_print_page_copies_code) s) <
             (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,15%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDgx_default_print_page_copies_i
             (Some (EAdd (EVar IDgx_default_print_page_copies_i)
             (ENum (-1))))),11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDgx_default_print_page_copies_i) s) >
             (eval (ENum (0)) s))%Z)),16%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDgx_default_print_page_copies_i) s) <=
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AAssign IDgx_default_print_page_copies_code None),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDgx_default_print_page_copies_z
             (Some (EAdd (ENum (1))
             (EVar IDgx_default_print_page_copies_z)))),21%positive)::
             (21%positive,AWeaken,7%positive)::nil
|}.

Definition gx_default_print_page_copies_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ 1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 4%positive => (1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ 1 * (s IDgx_default_print_page_copies_z) <= 0 /\ 1 * (s IDgx_default_print_page_copies_code) <= 0 /\ -1 * (s IDgx_default_print_page_copies_code) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_default_print_page_copies_code) <= 0 /\ 1 * (s IDgx_default_print_page_copies_code) <= 0 /\ 1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 8%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ 1 * (s IDgx_default_print_page_copies_code) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_code) <= 0)%Z
    | 10%positive => (-1 * (s IDgx_default_print_page_copies_code) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 11%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_code) <= 0)%Z
    | 12%positive => (-1 * (s IDgx_default_print_page_copies_code) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 13%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_code) <= 0)%Z
    | 14%positive => (-1 * (s IDgx_default_print_page_copies_code) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0 /\ 1 * (s IDgx_default_print_page_copies_i) <= 0)%Z
    | 15%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 16%positive => (-1 * (s IDgx_default_print_page_copies_code) <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDgx_default_print_page_copies_i) + 1 <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_code) <= 0)%Z
    | 18%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDgx_default_print_page_copies_i) + 1 <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) <= 0)%Z
    | 20%positive => (-1 * (s IDgx_default_print_page_copies_z) <= 0 /\ -1 * (s IDgx_default_print_page_copies_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDgx_default_print_page_copies_i) + 1 <= 0 /\ -1 * (s IDgx_default_print_page_copies_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gx_default_print_page_copies_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgx_default_print_page_copies_num_copies)))%Q
    | 2%positive => (max0((s IDgx_default_print_page_copies_num_copies))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 3%positive => (max0((s IDgx_default_print_page_copies__tmp))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 4%positive => (max0((s IDgx_default_print_page_copies_i))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 5%positive => (max0((s IDgx_default_print_page_copies_i))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 6%positive => (max0((s IDgx_default_print_page_copies_i))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 7%positive => (max0(-1 + (s IDgx_default_print_page_copies_i))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 8%positive => (max0(-1 + (s IDgx_default_print_page_copies_i))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 9%positive => (max0(-1 + (s IDgx_default_print_page_copies_i))
                     + max0((s IDgx_default_print_page_copies_z)))%Q
    | 10%positive => (max0(-1 + (s IDgx_default_print_page_copies_i))
                      + max0((s IDgx_default_print_page_copies_z)))%Q
    | 11%positive => (max0((s IDgx_default_print_page_copies_i))
                      + max0((s IDgx_default_print_page_copies_z)))%Q
    | 12%positive => (max0((s IDgx_default_print_page_copies_i))
                      + max0((s IDgx_default_print_page_copies_z)))%Q
    | 13%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 14%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 15%positive => ((s IDgx_default_print_page_copies_z))%Q
    | 16%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 17%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 18%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 19%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 20%positive => ((s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | 21%positive => (-(1 # 1) + (s IDgx_default_print_page_copies_z)
                      + max0((s IDgx_default_print_page_copies_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_default_print_page_copies_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgx_default_print_page_copies_i)) (-1
                                                                    + (s IDgx_default_print_page_copies_i)))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_ge_0 (-1
                                           + (s IDgx_default_print_page_copies_i));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_default_print_page_copies_z))) (F_check_ge ((s IDgx_default_print_page_copies_z)) (0))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgx_default_print_page_copies_z))) (F_check_ge ((s IDgx_default_print_page_copies_z)) (0))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgx_default_print_page_copies_i)) (-1
                                                                    + (s IDgx_default_print_page_copies_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDgx_default_print_page_copies_i))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_pre_decrement ((s IDgx_default_print_page_copies_i)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgx_default_print_page_copies_z)) (0))) (F_max0_ge_0 ((s IDgx_default_print_page_copies_z)))]
    | _ => []
  end.


Theorem gx_default_print_page_copies_ai_correct:
  forall s p' s', steps (g_start gx_default_print_page_copies) s (g_edges gx_default_print_page_copies) p' s' -> gx_default_print_page_copies_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_default_print_page_copies_pot_correct:
  forall s p' s',
    steps (g_start gx_default_print_page_copies) s (g_edges gx_default_print_page_copies) p' s' ->
    (gx_default_print_page_copies_pot (g_start gx_default_print_page_copies) s >= gx_default_print_page_copies_pot p' s')%Q.
Proof.
  check_lp gx_default_print_page_copies_ai_correct gx_default_print_page_copies_hints.
Qed.

