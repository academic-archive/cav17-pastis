Require Import pasta.Pasta.

Notation IDprepare_range_limit_table_z := 1%positive.
Notation IDprepare_range_limit_table_i := 2%positive.
Notation IDprepare_range_limit_table_cinfo := 3%positive.
Definition prepare_range_limit_table : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDprepare_range_limit_table_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDprepare_range_limit_table_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDprepare_range_limit_table_i) s) <=
             (eval (ENum (255)) s))%Z)),20%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDprepare_range_limit_table_i) s) >
             (eval (ENum (255)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDprepare_range_limit_table_i
             (Some (ENum (128)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDprepare_range_limit_table_i) s) <
             (eval (ENum (512)) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDprepare_range_limit_table_i) s) >=
             (eval (ENum (512)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDprepare_range_limit_table_i
             (Some (EAdd (EVar IDprepare_range_limit_table_i) (ENum (1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDprepare_range_limit_table_z
             (Some (EAdd (ENum (1)) (EVar IDprepare_range_limit_table_z)))),
             19%positive)::(19%positive,AWeaken,10%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDprepare_range_limit_table_i
             (Some (EAdd (EVar IDprepare_range_limit_table_i) (ENum (1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDprepare_range_limit_table_z
             (Some (EAdd (ENum (1)) (EVar IDprepare_range_limit_table_z)))),
             26%positive)::(26%positive,AWeaken,5%positive)::nil
|}.

Definition prepare_range_limit_table_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0)%Z
    | 3%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) <= 0)%Z
    | 4%positive => (-1 * (s IDprepare_range_limit_table_i) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) <= 0 /\ 1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0)%Z
    | 5%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -256 <= 0)%Z
    | 6%positive => (1 * (s IDprepare_range_limit_table_i) + -256 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 256 <= 0)%Z
    | 7%positive => (-1 * (s IDprepare_range_limit_table_i) + 256 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -256 <= 0)%Z
    | 8%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -128 <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 128 <= 0)%Z
    | 9%positive => (-1 * (s IDprepare_range_limit_table_i) + 128 <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -128 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0)%Z
    | 10%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 128 <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -512 <= 0)%Z
    | 11%positive => (1 * (s IDprepare_range_limit_table_i) + -512 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 512 <= 0)%Z
    | 12%positive => (-1 * (s IDprepare_range_limit_table_i) + 512 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -512 <= 0)%Z
    | 13%positive => (-1 * (s IDprepare_range_limit_table_i) + 128 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -511 <= 0)%Z
    | 14%positive => (1 * (s IDprepare_range_limit_table_i) + -511 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 128 <= 0)%Z
    | 15%positive => (-1 * (s IDprepare_range_limit_table_i) + 128 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -511 <= 0)%Z
    | 16%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 129 <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -512 <= 0)%Z
    | 17%positive => (1 * (s IDprepare_range_limit_table_i) + -512 <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 129 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0)%Z
    | 18%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 129 <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -512 <= 0)%Z
    | 19%positive => (1 * (s IDprepare_range_limit_table_i) + -512 <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 129 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDprepare_range_limit_table_i) <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -255 <= 0)%Z
    | 21%positive => (1 * (s IDprepare_range_limit_table_i) + -255 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) <= 0)%Z
    | 22%positive => (-1 * (s IDprepare_range_limit_table_i) <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -255 <= 0)%Z
    | 23%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 1 <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -256 <= 0)%Z
    | 24%positive => (1 * (s IDprepare_range_limit_table_i) + -256 <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 1 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) <= 0)%Z
    | 25%positive => (-1 * (s IDprepare_range_limit_table_z) <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 1 <= 0 /\ 1 * (s IDprepare_range_limit_table_i) + -256 <= 0)%Z
    | 26%positive => (1 * (s IDprepare_range_limit_table_i) + -256 <= 0 /\ -1 * (s IDprepare_range_limit_table_i) + 1 <= 0 /\ -1 * (s IDprepare_range_limit_table_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition prepare_range_limit_table_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((640 # 1))%Q
    | 2%positive => ((640 # 1) + (s IDprepare_range_limit_table_z))%Q
    | 3%positive => ((384 # 1) + (s IDprepare_range_limit_table_z)
                     + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 4%positive => ((384 # 1) + (s IDprepare_range_limit_table_z)
                     + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 5%positive => ((384 # 1) + (s IDprepare_range_limit_table_z)
                     + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 6%positive => ((384 # 1) + (s IDprepare_range_limit_table_z)
                     + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 7%positive => ((384 # 1) + (s IDprepare_range_limit_table_z))%Q
    | 8%positive => ((s IDprepare_range_limit_table_z)
                     + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 9%positive => ((s IDprepare_range_limit_table_z)
                     + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 10%positive => ((s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 11%positive => ((s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 12%positive => ((s IDprepare_range_limit_table_z))%Q
    | 13%positive => ((s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 14%positive => ((1 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(511 - (s IDprepare_range_limit_table_i)))%Q
    | 15%positive => ((1 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(511 - (s IDprepare_range_limit_table_i)))%Q
    | 16%positive => ((1 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 17%positive => ((1 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 18%positive => ((1 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 19%positive => ((s IDprepare_range_limit_table_z)
                      + max0(512 - (s IDprepare_range_limit_table_i)))%Q
    | 20%positive => ((384 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 21%positive => ((385 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(255 - (s IDprepare_range_limit_table_i)))%Q
    | 22%positive => ((385 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(255 - (s IDprepare_range_limit_table_i)))%Q
    | 23%positive => ((385 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 24%positive => ((385 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 25%positive => ((385 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | 26%positive => ((384 # 1) + (s IDprepare_range_limit_table_z)
                      + max0(256 - (s IDprepare_range_limit_table_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition prepare_range_limit_table_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                            - (s IDprepare_range_limit_table_i)) (255
                                                                    - (s IDprepare_range_limit_table_i)));
                     (*-1 0*) F_max0_ge_0 (255
                                           - (s IDprepare_range_limit_table_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                                             - (s IDprepare_range_limit_table_i)) (511
                                                                    - (s IDprepare_range_limit_table_i)));
                      (*-1 0*) F_max0_ge_0 (511
                                            - (s IDprepare_range_limit_table_i))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (512
                                                     - (s IDprepare_range_limit_table_i)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (256
                                                     - (s IDprepare_range_limit_table_i)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | _ => []
  end.


Theorem prepare_range_limit_table_ai_correct:
  forall s p' s', steps (g_start prepare_range_limit_table) s (g_edges prepare_range_limit_table) p' s' -> prepare_range_limit_table_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem prepare_range_limit_table_pot_correct:
  forall s p' s',
    steps (g_start prepare_range_limit_table) s (g_edges prepare_range_limit_table) p' s' ->
    (prepare_range_limit_table_pot (g_start prepare_range_limit_table) s >= prepare_range_limit_table_pot p' s')%Q.
Proof.
  check_lp prepare_range_limit_table_ai_correct prepare_range_limit_table_hints.
Qed.

