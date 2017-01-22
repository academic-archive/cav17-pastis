Require Import pasta.Pasta.

Notation IDjinit_forward_dct_z := 1%positive.
Notation IDjinit_forward_dct_i := 2%positive.
Notation IDjinit_forward_dct_cinfo := 3%positive.
Definition jinit_forward_dct : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDjinit_forward_dct_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,7%positive)::(3%positive,ANone,6%positive)::
             (3%positive,ANone,5%positive)::(3%positive,ANone,4%positive)::
             (4%positive,ANone,8%positive)::(5%positive,ANone,8%positive)::
             (6%positive,ANone,8%positive)::(7%positive,ANone,8%positive)::
             (8%positive,(AAssign IDjinit_forward_dct_i (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_forward_dct_i) s) <
             (eval (ENum (4)) s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_forward_dct_i) s) >=
             (eval (ENum (4)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDjinit_forward_dct_i
             (Some (EAdd (EVar IDjinit_forward_dct_i) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDjinit_forward_dct_z
             (Some (EAdd (ENum (1)) (EVar IDjinit_forward_dct_z)))),
             20%positive)::(20%positive,AWeaken,11%positive)::nil
|}.

Definition jinit_forward_dct_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 4%positive => (1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 5%positive => (1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 6%positive => (1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 7%positive => (1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 8%positive => (-1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 9%positive => (1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_i) <= 0 /\ -1 * (s IDjinit_forward_dct_i) <= 0)%Z
    | 10%positive => (-1 * (s IDjinit_forward_dct_i) <= 0 /\ 1 * (s IDjinit_forward_dct_i) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 11%positive => (-1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_i) <= 0 /\ 1 * (s IDjinit_forward_dct_i) + -4 <= 0)%Z
    | 12%positive => (1 * (s IDjinit_forward_dct_i) + -4 <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_i) + 4 <= 0)%Z
    | 13%positive => (-1 * (s IDjinit_forward_dct_i) + 4 <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_i) + -4 <= 0)%Z
    | 14%positive => (-1 * (s IDjinit_forward_dct_i) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_i) + -3 <= 0)%Z
    | 15%positive => (1 * (s IDjinit_forward_dct_i) + -3 <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_i) <= 0)%Z
    | 16%positive => (-1 * (s IDjinit_forward_dct_i) <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0 /\ 1 * (s IDjinit_forward_dct_i) + -3 <= 0)%Z
    | 17%positive => (-1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_i) + 1 <= 0 /\ 1 * (s IDjinit_forward_dct_i) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDjinit_forward_dct_i) + -4 <= 0 /\ -1 * (s IDjinit_forward_dct_i) + 1 <= 0 /\ -1 * (s IDjinit_forward_dct_z) <= 0)%Z
    | 19%positive => (-1 * (s IDjinit_forward_dct_z) <= 0 /\ -1 * (s IDjinit_forward_dct_i) + 1 <= 0 /\ 1 * (s IDjinit_forward_dct_i) + -4 <= 0)%Z
    | 20%positive => (1 * (s IDjinit_forward_dct_i) + -4 <= 0 /\ -1 * (s IDjinit_forward_dct_i) + 1 <= 0 /\ -1 * (s IDjinit_forward_dct_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jinit_forward_dct_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 3%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 4%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 5%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 6%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 7%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 8%positive => ((4 # 1) + (s IDjinit_forward_dct_z))%Q
    | 9%positive => ((s IDjinit_forward_dct_z)
                     + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 10%positive => ((s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 11%positive => ((s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 12%positive => ((s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 13%positive => ((s IDjinit_forward_dct_z))%Q
    | 14%positive => ((s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 15%positive => ((1 # 1) + (s IDjinit_forward_dct_z)
                      + max0(3 - (s IDjinit_forward_dct_i)))%Q
    | 16%positive => ((1 # 1) + (s IDjinit_forward_dct_z)
                      + max0(3 - (s IDjinit_forward_dct_i)))%Q
    | 17%positive => ((1 # 1) + (s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 18%positive => ((1 # 1) + (s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 19%positive => ((1 # 1) + (s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | 20%positive => ((s IDjinit_forward_dct_z)
                      + max0(4 - (s IDjinit_forward_dct_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jinit_forward_dct_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDjinit_forward_dct_i)) (3
                                                                    - (s IDjinit_forward_dct_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDjinit_forward_dct_i))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDjinit_forward_dct_i)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem jinit_forward_dct_ai_correct:
  forall s p' s', steps (g_start jinit_forward_dct) s (g_edges jinit_forward_dct) p' s' -> jinit_forward_dct_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jinit_forward_dct_pot_correct:
  forall s p' s',
    steps (g_start jinit_forward_dct) s (g_edges jinit_forward_dct) p' s' ->
    (jinit_forward_dct_pot (g_start jinit_forward_dct) s >= jinit_forward_dct_pot p' s')%Q.
Proof.
  check_lp jinit_forward_dct_ai_correct jinit_forward_dct_hints.
Qed.

