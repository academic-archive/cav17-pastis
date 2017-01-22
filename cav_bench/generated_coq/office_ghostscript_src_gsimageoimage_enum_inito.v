Require Import pasta.Pasta.

Notation IDimage_enum_init_z := 1%positive.
Notation IDimage_enum_init_i := 2%positive.
Notation IDimage_enum_init_pie := 3%positive.
Definition image_enum_init : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDimage_enum_init_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDimage_enum_init_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDimage_enum_init_i (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDimage_enum_init_i)
             s) < (eval (ENum (4)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDimage_enum_init_i)
             s) >= (eval (ENum (4)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDimage_enum_init_i
             (Some (EAdd (EVar IDimage_enum_init_i) (ENum (1))))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDimage_enum_init_z (Some (EAdd (ENum (1))
             (EVar IDimage_enum_init_z)))),16%positive)::
             (16%positive,AWeaken,7%positive)::nil
|}.

Definition image_enum_init_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0)%Z
    | 3%positive => (-1 * (s IDimage_enum_init_z) <= 0 /\ 1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_i) <= 0)%Z
    | 4%positive => (-1 * (s IDimage_enum_init_i) <= 0 /\ 1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0)%Z
    | 5%positive => (-1 * (s IDimage_enum_init_z) <= 0 /\ 1 * (s IDimage_enum_init_z) <= 0 /\ 1 * (s IDimage_enum_init_i) <= 0 /\ -1 * (s IDimage_enum_init_i) <= 0)%Z
    | 6%positive => (-1 * (s IDimage_enum_init_i) <= 0 /\ 1 * (s IDimage_enum_init_i) <= 0 /\ 1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0)%Z
    | 7%positive => (-1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_i) <= 0 /\ 1 * (s IDimage_enum_init_i) + -4 <= 0)%Z
    | 8%positive => (1 * (s IDimage_enum_init_i) + -4 <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_i) + 4 <= 0)%Z
    | 9%positive => (-1 * (s IDimage_enum_init_i) + 4 <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0 /\ 1 * (s IDimage_enum_init_i) + -4 <= 0)%Z
    | 10%positive => (-1 * (s IDimage_enum_init_i) <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0 /\ 1 * (s IDimage_enum_init_i) + -3 <= 0)%Z
    | 11%positive => (1 * (s IDimage_enum_init_i) + -3 <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_i) <= 0)%Z
    | 12%positive => (-1 * (s IDimage_enum_init_i) <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0 /\ 1 * (s IDimage_enum_init_i) + -3 <= 0)%Z
    | 13%positive => (-1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_i) + 1 <= 0 /\ 1 * (s IDimage_enum_init_i) + -4 <= 0)%Z
    | 14%positive => (1 * (s IDimage_enum_init_i) + -4 <= 0 /\ -1 * (s IDimage_enum_init_i) + 1 <= 0 /\ -1 * (s IDimage_enum_init_z) <= 0)%Z
    | 15%positive => (-1 * (s IDimage_enum_init_z) <= 0 /\ -1 * (s IDimage_enum_init_i) + 1 <= 0 /\ 1 * (s IDimage_enum_init_i) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDimage_enum_init_i) + -4 <= 0 /\ -1 * (s IDimage_enum_init_i) + 1 <= 0 /\ -1 * (s IDimage_enum_init_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition image_enum_init_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDimage_enum_init_z))%Q
    | 3%positive => ((4 # 1) + (s IDimage_enum_init_z))%Q
    | 4%positive => ((4 # 1) + (s IDimage_enum_init_z))%Q
    | 5%positive => ((s IDimage_enum_init_z)
                     + max0(4 - (s IDimage_enum_init_i)))%Q
    | 6%positive => ((s IDimage_enum_init_z)
                     + max0(4 - (s IDimage_enum_init_i)))%Q
    | 7%positive => ((s IDimage_enum_init_z)
                     + max0(4 - (s IDimage_enum_init_i)))%Q
    | 8%positive => ((s IDimage_enum_init_z)
                     + max0(4 - (s IDimage_enum_init_i)))%Q
    | 9%positive => ((s IDimage_enum_init_z))%Q
    | 10%positive => ((s IDimage_enum_init_z)
                      + max0(4 - (s IDimage_enum_init_i)))%Q
    | 11%positive => ((1 # 1) + (s IDimage_enum_init_z)
                      + max0(3 - (s IDimage_enum_init_i)))%Q
    | 12%positive => ((1 # 1) + (s IDimage_enum_init_z)
                      + max0(3 - (s IDimage_enum_init_i)))%Q
    | 13%positive => ((1 # 1) + (s IDimage_enum_init_z)
                      + max0(4 - (s IDimage_enum_init_i)))%Q
    | 14%positive => ((1 # 1) + (s IDimage_enum_init_z)
                      + max0(4 - (s IDimage_enum_init_i)))%Q
    | 15%positive => ((1 # 1) + (s IDimage_enum_init_z)
                      + max0(4 - (s IDimage_enum_init_i)))%Q
    | 16%positive => ((s IDimage_enum_init_z)
                      + max0(4 - (s IDimage_enum_init_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition image_enum_init_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                            - (s IDimage_enum_init_i)) (3
                                                                    - (s IDimage_enum_init_i)));
                     (*-1 0*) F_max0_ge_0 (3 - (s IDimage_enum_init_i))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDimage_enum_init_i)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem image_enum_init_ai_correct:
  forall s p' s', steps (g_start image_enum_init) s (g_edges image_enum_init) p' s' -> image_enum_init_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem image_enum_init_pot_correct:
  forall s p' s',
    steps (g_start image_enum_init) s (g_edges image_enum_init) p' s' ->
    (image_enum_init_pot (g_start image_enum_init) s >= image_enum_init_pot p' s')%Q.
Proof.
  check_lp image_enum_init_ai_correct image_enum_init_hints.
Qed.

