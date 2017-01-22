Require Import pasta.Pasta.

Notation IDs_MTF_init_z := 1%positive.
Notation IDs_MTF_init_i := 2%positive.
Notation IDs_MTF_init_st := 3%positive.
Definition s_MTF_init : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDs_MTF_init_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDs_MTF_init_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDs_MTF_init_i) s) <
             (eval (ENum (256)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDs_MTF_init_i) s) >=
             (eval (ENum (256)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDs_MTF_init_i
             (Some (EAdd (EVar IDs_MTF_init_i) (ENum (1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDs_MTF_init_z (Some (EAdd (ENum (1))
             (EVar IDs_MTF_init_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition s_MTF_init_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0)%Z
    | 3%positive => (-1 * (s IDs_MTF_init_z) <= 0 /\ 1 * (s IDs_MTF_init_z) <= 0 /\ 1 * (s IDs_MTF_init_i) <= 0 /\ -1 * (s IDs_MTF_init_i) <= 0)%Z
    | 4%positive => (-1 * (s IDs_MTF_init_i) <= 0 /\ 1 * (s IDs_MTF_init_i) <= 0 /\ 1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0)%Z
    | 5%positive => (-1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_i) <= 0 /\ 1 * (s IDs_MTF_init_i) + -256 <= 0)%Z
    | 6%positive => (1 * (s IDs_MTF_init_i) + -256 <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_i) + 256 <= 0)%Z
    | 7%positive => (-1 * (s IDs_MTF_init_i) + 256 <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0 /\ 1 * (s IDs_MTF_init_i) + -256 <= 0)%Z
    | 8%positive => (-1 * (s IDs_MTF_init_i) <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0 /\ 1 * (s IDs_MTF_init_i) + -255 <= 0)%Z
    | 9%positive => (1 * (s IDs_MTF_init_i) + -255 <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_i) <= 0)%Z
    | 10%positive => (-1 * (s IDs_MTF_init_i) <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0 /\ 1 * (s IDs_MTF_init_i) + -255 <= 0)%Z
    | 11%positive => (-1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_i) + 1 <= 0 /\ 1 * (s IDs_MTF_init_i) + -256 <= 0)%Z
    | 12%positive => (1 * (s IDs_MTF_init_i) + -256 <= 0 /\ -1 * (s IDs_MTF_init_i) + 1 <= 0 /\ -1 * (s IDs_MTF_init_z) <= 0)%Z
    | 13%positive => (-1 * (s IDs_MTF_init_z) <= 0 /\ -1 * (s IDs_MTF_init_i) + 1 <= 0 /\ 1 * (s IDs_MTF_init_i) + -256 <= 0)%Z
    | 14%positive => (1 * (s IDs_MTF_init_i) + -256 <= 0 /\ -1 * (s IDs_MTF_init_i) + 1 <= 0 /\ -1 * (s IDs_MTF_init_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition s_MTF_init_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDs_MTF_init_z))%Q
    | 3%positive => ((s IDs_MTF_init_z) + max0(256 - (s IDs_MTF_init_i)))%Q
    | 4%positive => ((s IDs_MTF_init_z) + max0(256 - (s IDs_MTF_init_i)))%Q
    | 5%positive => ((s IDs_MTF_init_z) + max0(256 - (s IDs_MTF_init_i)))%Q
    | 6%positive => ((s IDs_MTF_init_z) + max0(256 - (s IDs_MTF_init_i)))%Q
    | 7%positive => ((s IDs_MTF_init_z))%Q
    | 8%positive => ((s IDs_MTF_init_z) + max0(256 - (s IDs_MTF_init_i)))%Q
    | 9%positive => ((1 # 1) + (s IDs_MTF_init_z)
                     + max0(255 - (s IDs_MTF_init_i)))%Q
    | 10%positive => ((1 # 1) + (s IDs_MTF_init_z)
                      + max0(255 - (s IDs_MTF_init_i)))%Q
    | 11%positive => ((1 # 1) + (s IDs_MTF_init_z)
                      + max0(256 - (s IDs_MTF_init_i)))%Q
    | 12%positive => ((1 # 1) + (s IDs_MTF_init_z)
                      + max0(256 - (s IDs_MTF_init_i)))%Q
    | 13%positive => ((1 # 1) + (s IDs_MTF_init_z)
                      + max0(256 - (s IDs_MTF_init_i)))%Q
    | 14%positive => ((s IDs_MTF_init_z) + max0(256 - (s IDs_MTF_init_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition s_MTF_init_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                            - (s IDs_MTF_init_i)) (255
                                                                    - (s IDs_MTF_init_i)));
                     (*-1 0*) F_max0_ge_0 (255 - (s IDs_MTF_init_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (256 - (s IDs_MTF_init_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem s_MTF_init_ai_correct:
  forall s p' s', steps (g_start s_MTF_init) s (g_edges s_MTF_init) p' s' -> s_MTF_init_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem s_MTF_init_pot_correct:
  forall s p' s',
    steps (g_start s_MTF_init) s (g_edges s_MTF_init) p' s' ->
    (s_MTF_init_pot (g_start s_MTF_init) s >= s_MTF_init_pot p' s')%Q.
Proof.
  check_lp s_MTF_init_ai_correct s_MTF_init_hints.
Qed.

