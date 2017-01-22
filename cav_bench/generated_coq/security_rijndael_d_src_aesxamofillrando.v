Require Import pasta.Pasta.

Notation IDfillrand_z := 1%positive.
Notation IDfillrand__tmp := 2%positive.
Notation IDfillrand_fillrand.count := 3%positive.
Notation IDfillrand_fillrand.mt := 4%positive.
Notation IDfillrand_i := 5%positive.
Notation IDfillrand_buf := 6%positive.
Notation IDfillrand_len := 7%positive.
Definition fillrand : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDfillrand_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDfillrand__tmp
             (Some (EVar IDfillrand_len))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDfillrand_fillrand.mt) s) <>
             (eval (ENum (0)) s))%Z)),6%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDfillrand_fillrand.mt) s) =
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,9%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDfillrand_fillrand.mt (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDfillrand_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDfillrand_i) s) <
             (eval (EVar IDfillrand__tmp) s))%Z)),15%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDfillrand_i) s) >=
             (eval (EVar IDfillrand__tmp) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDfillrand_fillrand.count) s) =
             (eval (ENum (4)) s))%Z)),18%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDfillrand_fillrand.count) s) <>
             (eval (ENum (4)) s))%Z)),17%positive)::
             (17%positive,AWeaken,21%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDfillrand_fillrand.count
             (Some (ENum (0)))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDfillrand_fillrand.count
             (Some (EAdd (EVar IDfillrand_fillrand.count) (ENum (1))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDfillrand_i
             (Some (EAdd (EVar IDfillrand_i) (ENum (1))))),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDfillrand_z (Some (EAdd (ENum (1))
             (EVar IDfillrand_z)))),27%positive)::
             (27%positive,AWeaken,12%positive)::nil
|}.

Definition fillrand_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_z) <= 0)%Z
    | 4%positive => (1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 6%positive => (-1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_z) <= 0)%Z
    | 7%positive => (1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_z) <= 0)%Z
    | 8%positive => (-1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 9%positive => (-1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_z) <= 0)%Z
    | 10%positive => (-1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_i) <= 0)%Z
    | 11%positive => (-1 * (s IDfillrand_i) <= 0 /\ 1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_z) <= 0)%Z
    | 12%positive => (-1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 13%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ 1 * (s IDfillrand__tmp)+ -1 * (s IDfillrand_i) <= 0)%Z
    | 14%positive => (1 * (s IDfillrand__tmp)+ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 15%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 17%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0 /\ 1 * (s IDfillrand_fillrand.count) + -4 <= 0 /\ -1 * (s IDfillrand_fillrand.count) + 4 <= 0)%Z
    | 19%positive => (-1 * (s IDfillrand_fillrand.count) + 4 <= 0 /\ 1 * (s IDfillrand_fillrand.count) + -4 <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 20%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0 /\ 1 * (s IDfillrand_fillrand.count) <= 0 /\ -1 * (s IDfillrand_fillrand.count) <= 0)%Z
    | 21%positive => (-1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 22%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) + 1 <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 24%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDfillrand_i) + 1 <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0)%Z
    | 26%positive => (1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_z) <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDfillrand_i) + 1 <= 0 /\ -1 * (s IDfillrand__tmp)+ 1 * (s IDfillrand_i) <= 0 /\ -1 * (s IDfillrand_fillrand.mt) <= 0 /\ 1 * (s IDfillrand_fillrand.mt) <= 0 /\ -1 * (s IDfillrand_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition fillrand_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDfillrand_len)))%Q
    | 2%positive => ((s IDfillrand_z) + max0((s IDfillrand_len)))%Q
    | 3%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 4%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 5%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 6%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 7%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 8%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 9%positive => ((s IDfillrand_z) + max0((s IDfillrand__tmp)))%Q
    | 10%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 11%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 12%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 13%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 14%positive => ((s IDfillrand_z))%Q
    | 15%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 16%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 17%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 18%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 19%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0(-1 + (s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 20%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0(-1 + (s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 21%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0(-1 + (s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 22%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0(-1 + (s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 23%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0(-1 + (s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 24%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 25%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 26%positive => ((1 # 1) + (s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | 27%positive => ((s IDfillrand_z)
                      + max0((s IDfillrand__tmp) - (s IDfillrand_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition fillrand_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDfillrand__tmp)
                                                             - (s IDfillrand_i)) (-1
                                                                    + (s IDfillrand__tmp)
                                                                    - (s IDfillrand_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDfillrand__tmp)
                                            - (s IDfillrand_i))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_pre_decrement ((s IDfillrand__tmp)
                                                    - (s IDfillrand_i)) (1)]
    | 18%positive => [(*-1 0*) F_max0_pre_decrement ((s IDfillrand__tmp)
                                                     - (s IDfillrand_i)) (1)]
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


Theorem fillrand_ai_correct:
  forall s p' s', steps (g_start fillrand) s (g_edges fillrand) p' s' -> fillrand_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem fillrand_pot_correct:
  forall s p' s',
    steps (g_start fillrand) s (g_edges fillrand) p' s' ->
    (fillrand_pot (g_start fillrand) s >= fillrand_pot p' s')%Q.
Proof.
  check_lp fillrand_ai_correct fillrand_hints.
Qed.

