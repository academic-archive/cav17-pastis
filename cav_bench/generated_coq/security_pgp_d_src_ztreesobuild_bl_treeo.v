Require Import pasta.Pasta.

Notation IDbuild_bl_tree_z := 1%positive.
Notation IDbuild_bl_tree_max_blindex := 2%positive.
Notation IDbuild_bl_tree_opt_len := 3%positive.
Definition build_bl_tree : graph := {|
  g_start := 1%positive;
  g_end := 18%positive;
  g_edges := (1%positive,(AAssign IDbuild_bl_tree_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDbuild_bl_tree_max_blindex
             (Some (ENum (18)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDbuild_bl_tree_max_blindex) s) >=
             (eval (ENum (3)) s))%Z)),7%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDbuild_bl_tree_max_blindex) s) <
             (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,16%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,15%positive)::(8%positive,ANone,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDbuild_bl_tree_max_blindex
             (Some (EAdd (EVar IDbuild_bl_tree_max_blindex) (ENum (-1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDbuild_bl_tree_z (Some (EAdd (ENum (1))
             (EVar IDbuild_bl_tree_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDbuild_bl_tree_opt_len
             (Some (EAdd (EVar IDbuild_bl_tree_opt_len)
             (EAdd (EAdd (EAdd (EMul (ENum (3))
             (EAdd (EVar IDbuild_bl_tree_max_blindex) (ENum (1))))
             (ENum (5))) (ENum (5))) (ENum (4)))))),17%positive)::
             (17%positive,AWeaken,18%positive)::nil
|}.

Definition build_bl_tree_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 18 <= 0)%Z
    | 4%positive => (-1 * (s IDbuild_bl_tree_max_blindex) + 18 <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ 1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0)%Z
    | 6%positive => (-1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -2 <= 0)%Z
    | 7%positive => (1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 3 <= 0)%Z
    | 8%positive => (-1 * (s IDbuild_bl_tree_max_blindex) + 3 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0)%Z
    | 9%positive => (1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 3 <= 0)%Z
    | 10%positive => (-1 * (s IDbuild_bl_tree_max_blindex) + 3 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0)%Z
    | 11%positive => (-1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -17 <= 0)%Z
    | 12%positive => (1 * (s IDbuild_bl_tree_max_blindex) + -17 <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0)%Z
    | 13%positive => (-1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -17 <= 0)%Z
    | 14%positive => (1 * (s IDbuild_bl_tree_max_blindex) + -17 <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ -1 * (s IDbuild_bl_tree_z) + 1 <= 0)%Z
    | 15%positive => (1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 3 <= 0)%Z
    | 16%positive => (-1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0)%Z
    | 17%positive => (1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ -1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0)%Z
    | 18%positive => (-1 * (s IDbuild_bl_tree_max_blindex) + 2 <= 0 /\ -1 * (s IDbuild_bl_tree_z) <= 0 /\ 1 * (s IDbuild_bl_tree_max_blindex) + -18 <= 0)%Z
    | _ => False
  end.

Definition build_bl_tree_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDbuild_bl_tree_z))%Q
    | 3%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 4%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 5%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 6%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 7%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 8%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 9%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                     + (s IDbuild_bl_tree_z))%Q
    | 10%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 11%positive => (-(1 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 12%positive => (-(1 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 13%positive => (-(1 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 14%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 15%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 16%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 17%positive => (-(2 # 1) + (s IDbuild_bl_tree_max_blindex)
                      + (s IDbuild_bl_tree_z))%Q
    | 18%positive => ((s IDbuild_bl_tree_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition build_bl_tree_hints (p : node) (s : state) := 
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
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-2
                                                             + (s IDbuild_bl_tree_max_blindex)) (-3
                                                                    + (s IDbuild_bl_tree_max_blindex)));
                      (*-1 0*) F_max0_ge_0 (-3
                                            + (s IDbuild_bl_tree_max_blindex));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + (s IDbuild_bl_tree_max_blindex)) (0))) (F_max0_ge_0 (-2
                                                                    + (s IDbuild_bl_tree_max_blindex)))]
    | 18%positive => []
    | _ => []
  end.


Theorem build_bl_tree_ai_correct:
  forall s p' s', steps (g_start build_bl_tree) s (g_edges build_bl_tree) p' s' -> build_bl_tree_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem build_bl_tree_pot_correct:
  forall s p' s',
    steps (g_start build_bl_tree) s (g_edges build_bl_tree) p' s' ->
    (build_bl_tree_pot (g_start build_bl_tree) s >= build_bl_tree_pot p' s')%Q.
Proof.
  check_lp build_bl_tree_ai_correct build_bl_tree_hints.
Qed.

