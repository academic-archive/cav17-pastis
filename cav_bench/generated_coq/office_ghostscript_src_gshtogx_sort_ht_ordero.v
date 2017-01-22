Require Import pasta.Pasta.

Notation IDgx_sort_ht_order_z := 1%positive.
Notation IDgx_sort_ht_order__tmp := 2%positive.
Notation IDgx_sort_ht_order_i := 3%positive.
Notation IDgx_sort_ht_order_N := 4%positive.
Notation IDgx_sort_ht_order_recs := 5%positive.
Definition gx_sort_ht_order : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDgx_sort_ht_order_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDgx_sort_ht_order_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDgx_sort_ht_order__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDgx_sort_ht_order__tmp
             (Some (EVar IDgx_sort_ht_order_N))),6%positive)::
             (6%positive,(AAssign IDgx_sort_ht_order_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDgx_sort_ht_order_i)
             s) < (eval (EVar IDgx_sort_ht_order__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDgx_sort_ht_order_i)
             s) >= (eval (EVar IDgx_sort_ht_order__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDgx_sort_ht_order_i
             (Some (EAdd (EVar IDgx_sort_ht_order_i) (ENum (1))))),
             15%positive)::(15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDgx_sort_ht_order_z
             (Some (EAdd (ENum (1)) (EVar IDgx_sort_ht_order_z)))),
             18%positive)::(18%positive,AWeaken,9%positive)::nil
|}.

Definition gx_sort_ht_order_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgx_sort_ht_order_z) <= 0 /\ 1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 4%positive => (-1 * (s IDgx_sort_ht_order_i) <= 0 /\ 1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDgx_sort_ht_order__tmp) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ 1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 6%positive => (-1 * (s IDgx_sort_ht_order_i) <= 0 /\ 1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0)%Z
    | 7%positive => (-1 * (s IDgx_sort_ht_order_z) <= 0 /\ 1 * (s IDgx_sort_ht_order_z) <= 0 /\ 1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 8%positive => (-1 * (s IDgx_sort_ht_order_i) <= 0 /\ 1 * (s IDgx_sort_ht_order_i) <= 0 /\ 1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0)%Z
    | 9%positive => (-1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 10%positive => (-1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ 1 * (s IDgx_sort_ht_order__tmp)+ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 11%positive => (1 * (s IDgx_sort_ht_order__tmp)+ -1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 12%positive => (-1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) + 1 <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 14%positive => (-1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) + 1 <= 0 /\ -1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 16%positive => (-1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) + 1 <= 0 /\ -1 * (s IDgx_sort_ht_order_z) <= 0)%Z
    | 17%positive => (-1 * (s IDgx_sort_ht_order_z) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) + 1 <= 0 /\ -1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) <= 0)%Z
    | 18%positive => (-1 * (s IDgx_sort_ht_order__tmp)+ 1 * (s IDgx_sort_ht_order_i) <= 0 /\ -1 * (s IDgx_sort_ht_order_i) + 1 <= 0 /\ -1 * (s IDgx_sort_ht_order_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gx_sort_ht_order_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDgx_sort_ht_order_N)))%Q
    | 2%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order_N)))%Q
    | 3%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order_N)))%Q
    | 4%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order_N)))%Q
    | 5%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order_N)))%Q
    | 6%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order__tmp)))%Q
    | 7%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order__tmp)
                            - (s IDgx_sort_ht_order_i)))%Q
    | 8%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order__tmp)
                            - (s IDgx_sort_ht_order_i)))%Q
    | 9%positive => ((s IDgx_sort_ht_order_z)
                     + max0((s IDgx_sort_ht_order__tmp)
                            - (s IDgx_sort_ht_order_i)))%Q
    | 10%positive => ((s IDgx_sort_ht_order_z)
                      + max0((s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 11%positive => ((s IDgx_sort_ht_order_z))%Q
    | 12%positive => ((s IDgx_sort_ht_order_z)
                      + max0((s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 13%positive => ((1 # 1) + (s IDgx_sort_ht_order_z)
                      + max0(-1 + (s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 14%positive => ((1 # 1) + (s IDgx_sort_ht_order_z)
                      + max0(-1 + (s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 15%positive => ((1 # 1) + (s IDgx_sort_ht_order_z)
                      + max0((s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 16%positive => ((1 # 1) + (s IDgx_sort_ht_order_z)
                      + max0((s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 17%positive => ((1 # 1) + (s IDgx_sort_ht_order_z)
                      + max0((s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | 18%positive => ((s IDgx_sort_ht_order_z)
                      + max0((s IDgx_sort_ht_order__tmp)
                             - (s IDgx_sort_ht_order_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gx_sort_ht_order_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDgx_sort_ht_order__tmp)
                                                             - (s IDgx_sort_ht_order_i)) (-1
                                                                    + (s IDgx_sort_ht_order__tmp)
                                                                    - (s IDgx_sort_ht_order_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDgx_sort_ht_order__tmp)
                                            - (s IDgx_sort_ht_order_i))]
    | 11%positive => []
    | 12%positive => [(*0 1*) F_max0_pre_decrement ((s IDgx_sort_ht_order__tmp)
                                                    - (s IDgx_sort_ht_order_i)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | _ => []
  end.


Theorem gx_sort_ht_order_ai_correct:
  forall s p' s', steps (g_start gx_sort_ht_order) s (g_edges gx_sort_ht_order) p' s' -> gx_sort_ht_order_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gx_sort_ht_order_pot_correct:
  forall s p' s',
    steps (g_start gx_sort_ht_order) s (g_edges gx_sort_ht_order) p' s' ->
    (gx_sort_ht_order_pot (g_start gx_sort_ht_order) s >= gx_sort_ht_order_pot p' s')%Q.
Proof.
  check_lp gx_sort_ht_order_ai_correct gx_sort_ht_order_hints.
Qed.

