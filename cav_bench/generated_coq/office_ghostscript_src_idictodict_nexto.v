Require Import pasta.Pasta.

Notation IDdict_next_z := 1%positive.
Notation IDdict_next__tmp := 2%positive.
Notation IDdict_next__tmp1 := 3%positive.
Notation IDdict_next_eltp := 4%positive.
Notation IDdict_next_index := 5%positive.
Notation IDdict_next_pdref := 6%positive.
Definition dict_next : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDdict_next_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDdict_next__tmp
             (Some (EVar IDdict_next_index))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDdict_next__tmp
             (Some (EAdd (EVar IDdict_next__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDdict_next__tmp) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),11%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDdict_next__tmp) (ENum (-1))) s) <
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDdict_next__tmp1 (Some (ENum (-1)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,25%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,20%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,17%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (16%positive,ANone,20%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDdict_next_z (Some (EAdd (ENum (1))
             (EVar IDdict_next_z)))),4%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDdict_next__tmp1
             (Some (EVar IDdict_next__tmp))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::nil
|}.

Definition dict_next_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdict_next_z) <= 0 /\ 1 * (s IDdict_next_z) <= 0)%Z
    | 4%positive => (-1 * (s IDdict_next_z) <= 0)%Z
    | 5%positive => (-1 * (s IDdict_next_z) <= 0)%Z
    | 6%positive => (-1 * (s IDdict_next_z) <= 0)%Z
    | 7%positive => (-1 * (s IDdict_next_z) <= 0 /\ 1 * (s IDdict_next__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDdict_next__tmp) <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 9%positive => (-1 * (s IDdict_next_z) <= 0 /\ 1 * (s IDdict_next__tmp) <= 0 /\ 1 * (s IDdict_next__tmp1) + 1 <= 0 /\ -1 * (s IDdict_next__tmp1) + -1 <= 0)%Z
    | 10%positive => (-1 * (s IDdict_next__tmp1) + -1 <= 0 /\ 1 * (s IDdict_next__tmp1) + 1 <= 0 /\ 1 * (s IDdict_next__tmp) <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 11%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDdict_next__tmp) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 13%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDdict_next__tmp) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 15%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDdict_next__tmp) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 17%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDdict_next__tmp) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 19%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDdict_next__tmp) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | 22%positive => (-1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDdict_next__tmp) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp1) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDdict_next__tmp1) + 1 <= 0 /\ -1 * (s IDdict_next_z) <= 0 /\ -1 * (s IDdict_next__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDdict_next__tmp1) + -1 <= 0 /\ -1 * (s IDdict_next_z) <= 0)%Z
    | _ => False
  end.

Definition dict_next_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 2) * max0(-1 + (s IDdict_next_index))
                     + (1 # 2) * max0((s IDdict_next_index)))%Q
    | 2%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(-1 + (s IDdict_next_index))
                     + (1 # 2) * max0((s IDdict_next_index)))%Q
    | 3%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 4%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 5%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 6%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 7%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 8%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 9%positive => ((s IDdict_next_z)
                     + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                     + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 10%positive => ((s IDdict_next_z)
                      + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 11%positive => ((s IDdict_next_z)
                      + (1 # 2) * max0(1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 22%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 23%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 24%positive => ((1 # 1) + (s IDdict_next_z)
                      + (1 # 2) * max0(-1 + (s IDdict_next__tmp))
                      + (1 # 2) * max0((s IDdict_next__tmp)))%Q
    | 25%positive => ((s IDdict_next_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition dict_next_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-0.5 0*) F_max0_monotonic (F_check_ge ((s IDdict_next__tmp)) (-1
                                                                    + (s IDdict_next__tmp)));
                      (*-0.5 0*) F_max0_ge_0 (-1 + (s IDdict_next__tmp));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                   + 
                                                                   (s IDdict_next__tmp))) (F_check_ge (0) (0))]
    | 11%positive => [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    (s IDdict_next__tmp))) (F_check_ge (1
                                                                    + (s IDdict_next__tmp)) (0));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDdict_next__tmp)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDdict_next__tmp)))]
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
    | 24%positive => [(*-1.5 0*) F_one;
                      (*-0.5 0*) F_max0_pre_decrement ((s IDdict_next__tmp)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDdict_next__tmp))]
    | 25%positive => []
    | _ => []
  end.


Theorem dict_next_ai_correct:
  forall s p' s', steps (g_start dict_next) s (g_edges dict_next) p' s' -> dict_next_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem dict_next_pot_correct:
  forall s p' s',
    steps (g_start dict_next) s (g_edges dict_next) p' s' ->
    (dict_next_pot (g_start dict_next) s >= dict_next_pot p' s')%Q.
Proof.
  check_lp dict_next_ai_correct dict_next_hints.
Qed.

