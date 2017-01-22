Require Import pasta.Pasta.

Notation IDparse_data_z := 1%positive.
Notation IDparse_data__tmp := 2%positive.
Notation IDparse_data__tmp1 := 3%positive.
Notation IDparse_data_frame_dref_off56 := 4%positive.
Notation IDparse_data_i := 5%positive.
Notation IDparse_data_data := 6%positive.
Notation IDparse_data_frame := 7%positive.
Notation IDparse_data_length := 8%positive.
Definition parse_data : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDparse_data_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDparse_data_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDparse_data_frame_dref_off56) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDparse_data__tmp1
             (Some (EVar IDparse_data_length))),6%positive)::
             (6%positive,(AAssign IDparse_data_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDparse_data_i) s) <
             (eval (EVar IDparse_data_frame_dref_off56) s))%Z)),14%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDparse_data_i) s) >=
             (eval (EVar IDparse_data_frame_dref_off56) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDparse_data__tmp (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,25%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,22%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDparse_data_i
             (Some (EAdd (EVar IDparse_data_i) (ENum (1))))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDparse_data_z (Some (EAdd (ENum (1))
             (EVar IDparse_data_z)))),21%positive)::
             (21%positive,AWeaken,9%positive)::
             (22%positive,(AAssign IDparse_data__tmp (Some (ENum (-1)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::nil
|}.

Definition parse_data_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_z) <= 0)%Z
    | 3%positive => (-1 * (s IDparse_data_z) <= 0 /\ 1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0)%Z
    | 4%positive => (-1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56) <= 0)%Z
    | 5%positive => (-1 * (s IDparse_data_frame_dref_off56) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ 1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0)%Z
    | 6%positive => (-1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56) <= 0)%Z
    | 7%positive => (-1 * (s IDparse_data_frame_dref_off56) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ 1 * (s IDparse_data_z) <= 0 /\ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_i) <= 0)%Z
    | 8%positive => (-1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56) <= 0)%Z
    | 9%positive => (-1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0)%Z
    | 10%positive => (-1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ 1 * (s IDparse_data_frame_dref_off56)+ -1 * (s IDparse_data_i) <= 0)%Z
    | 11%positive => (1 * (s IDparse_data_frame_dref_off56)+ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0)%Z
    | 12%positive => (-1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ 1 * (s IDparse_data_frame_dref_off56)+ -1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data__tmp) <= 0 /\ -1 * (s IDparse_data__tmp) <= 0)%Z
    | 13%positive => (-1 * (s IDparse_data__tmp) <= 0 /\ 1 * (s IDparse_data__tmp) <= 0 /\ 1 * (s IDparse_data_frame_dref_off56)+ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0)%Z
    | 14%positive => (-1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0)%Z
    | 16%positive => (-1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0)%Z
    | 18%positive => (-1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDparse_data_i) + 1 <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0)%Z
    | 20%positive => (-1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDparse_data_i) + 1 <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data__tmp) + 1 <= 0 /\ -1 * (s IDparse_data__tmp) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDparse_data__tmp) + -1 <= 0 /\ 1 * (s IDparse_data__tmp) + 1 <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDparse_data_frame_dref_off56)+ 1 * (s IDparse_data_i) <= 0 /\ 1 * (s IDparse_data__tmp) <= 0 /\ -1 * (s IDparse_data_z) <= 0 /\ -1 * (s IDparse_data_i) <= 0 /\ -1 * (s IDparse_data__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition parse_data_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDparse_data_frame_dref_off56)))%Q
    | 2%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)))%Q
    | 3%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)))%Q
    | 4%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)))%Q
    | 5%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)))%Q
    | 6%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)))%Q
    | 7%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)
                            - (s IDparse_data_i)))%Q
    | 8%positive => ((s IDparse_data_z)
                     + max0((s IDparse_data_frame_dref_off56)
                            - (s IDparse_data_i)))%Q
    | 9%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                     + (s IDparse_data_z))%Q
    | 10%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 11%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 12%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 13%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 14%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 15%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 16%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 17%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 18%positive => ((1 # 1) + (s IDparse_data_frame_dref_off56)
                      - (s IDparse_data_i) + (s IDparse_data_z))%Q
    | 19%positive => ((1 # 1) + (s IDparse_data_frame_dref_off56)
                      - (s IDparse_data_i) + (s IDparse_data_z))%Q
    | 20%positive => ((1 # 1) + (s IDparse_data_frame_dref_off56)
                      - (s IDparse_data_i) + (s IDparse_data_z))%Q
    | 21%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 22%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 23%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 24%positive => ((s IDparse_data_frame_dref_off56) - (s IDparse_data_i)
                      + (s IDparse_data_z))%Q
    | 25%positive => ((s IDparse_data_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition parse_data_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDparse_data_frame_dref_off56)
                                                                  - (s IDparse_data_i))) (F_check_ge ((s IDparse_data_frame_dref_off56)
                                                                    - (s IDparse_data_i)) (0))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDparse_data_frame_dref_off56)
                                                             - (s IDparse_data_i)) (-1
                                                                    + (s IDparse_data_frame_dref_off56)
                                                                    - (s IDparse_data_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDparse_data_frame_dref_off56)
                                            - (s IDparse_data_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_data_frame_dref_off56)
                                                                    - (s IDparse_data_i)) (0))) (F_max0_ge_0 ((s IDparse_data_frame_dref_off56)
                                                                    - (s IDparse_data_i)))]
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
    | 24%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDparse_data_frame_dref_off56)
                                                     - (s IDparse_data_i)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDparse_data_frame_dref_off56)
                                            - (s IDparse_data_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDparse_data_frame_dref_off56)
                                                                    - (s IDparse_data_i)) (0))) (F_max0_ge_0 ((s IDparse_data_frame_dref_off56)
                                                                    - (s IDparse_data_i)))]
    | 25%positive => []
    | _ => []
  end.


Theorem parse_data_ai_correct:
  forall s p' s', steps (g_start parse_data) s (g_edges parse_data) p' s' -> parse_data_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem parse_data_pot_correct:
  forall s p' s',
    steps (g_start parse_data) s (g_edges parse_data) p' s' ->
    (parse_data_pot (g_start parse_data) s >= parse_data_pot p' s')%Q.
Proof.
  check_lp parse_data_ai_correct parse_data_hints.
Qed.

