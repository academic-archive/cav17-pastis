Require Import pasta.Pasta.

Notation IDbuild_ycc_rgb_table_z := 1%positive.
Notation IDbuild_ycc_rgb_table_i := 2%positive.
Notation IDbuild_ycc_rgb_table_x := 3%positive.
Notation IDbuild_ycc_rgb_table_cinfo := 4%positive.
Definition build_ycc_rgb_table : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDbuild_ycc_rgb_table_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDbuild_ycc_rgb_table_i (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDbuild_ycc_rgb_table_x
             (Some (ENum (-128)))),4%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDbuild_ycc_rgb_table_i) s) <=
             (eval (ENum (255)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDbuild_ycc_rgb_table_i) s) >
             (eval (ENum (255)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDbuild_ycc_rgb_table_i
             (Some (EAdd (EVar IDbuild_ycc_rgb_table_i) (ENum (1))))),
             12%positive)::
             (12%positive,(AAssign IDbuild_ycc_rgb_table_x
             (Some (EAdd (EVar IDbuild_ycc_rgb_table_x) (ENum (1))))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDbuild_ycc_rgb_table_z
             (Some (EAdd (ENum (1)) (EVar IDbuild_ycc_rgb_table_z)))),
             16%positive)::(16%positive,AWeaken,6%positive)::nil
|}.

Definition build_ycc_rgb_table_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) <= 0)%Z
    | 4%positive => (-1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_x) + 128 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0)%Z
    | 5%positive => (-1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_x) + 128 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) <= 0)%Z
    | 6%positive => (-1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0)%Z
    | 7%positive => (1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) + 256 <= 0)%Z
    | 8%positive => (-1 * (s IDbuild_ycc_rgb_table_i) + 256 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0)%Z
    | 9%positive => (-1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -255 <= 0)%Z
    | 10%positive => (1 * (s IDbuild_ycc_rgb_table_i) + -255 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0)%Z
    | 11%positive => (-1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -255 <= 0)%Z
    | 12%positive => (-1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -128 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) + 1 <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0)%Z
    | 13%positive => (1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) + 1 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -127 <= 0)%Z
    | 14%positive => (-1 * (s IDbuild_ycc_rgb_table_x) + -127 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) + 1 <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0)%Z
    | 15%positive => (1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) + 1 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_x) + -127 <= 0)%Z
    | 16%positive => (-1 * (s IDbuild_ycc_rgb_table_x) + -127 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_i) + 1 <= 0 /\ 1 * (s IDbuild_ycc_rgb_table_i) + -256 <= 0 /\ -1 * (s IDbuild_ycc_rgb_table_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition build_ycc_rgb_table_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDbuild_ycc_rgb_table_z))%Q
    | 3%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                     + (s IDbuild_ycc_rgb_table_z))%Q
    | 4%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                     + (s IDbuild_ycc_rgb_table_z))%Q
    | 5%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                     + (s IDbuild_ycc_rgb_table_z))%Q
    | 6%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                     + (s IDbuild_ycc_rgb_table_z))%Q
    | 7%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                     + (s IDbuild_ycc_rgb_table_z))%Q
    | 8%positive => ((s IDbuild_ycc_rgb_table_z))%Q
    | 9%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                     + (s IDbuild_ycc_rgb_table_z))%Q
    | 10%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | 11%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | 12%positive => ((257 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | 13%positive => ((257 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | 14%positive => ((257 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | 15%positive => ((257 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | 16%positive => ((256 # 1) - (s IDbuild_ycc_rgb_table_i)
                      + (s IDbuild_ycc_rgb_table_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition build_ycc_rgb_table_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                            - (s IDbuild_ycc_rgb_table_i)) (255
                                                                    - (s IDbuild_ycc_rgb_table_i)));
                     (*-1 0*) F_max0_ge_0 (255 - (s IDbuild_ycc_rgb_table_i));
                     (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - (s IDbuild_ycc_rgb_table_i)) (0))) (F_max0_ge_0 (256
                                                                    - (s IDbuild_ycc_rgb_table_i)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem build_ycc_rgb_table_ai_correct:
  forall s p' s', steps (g_start build_ycc_rgb_table) s (g_edges build_ycc_rgb_table) p' s' -> build_ycc_rgb_table_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem build_ycc_rgb_table_pot_correct:
  forall s p' s',
    steps (g_start build_ycc_rgb_table) s (g_edges build_ycc_rgb_table) p' s' ->
    (build_ycc_rgb_table_pot (g_start build_ycc_rgb_table) s >= build_ycc_rgb_table_pot p' s')%Q.
Proof.
  check_lp build_ycc_rgb_table_ai_correct build_ycc_rgb_table_hints.
Qed.

