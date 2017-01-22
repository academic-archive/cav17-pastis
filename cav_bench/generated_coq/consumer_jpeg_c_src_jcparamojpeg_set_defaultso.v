Require Import pasta.Pasta.

Notation IDjpeg_set_defaults_z := 1%positive.
Notation IDjpeg_set_defaults_i := 2%positive.
Notation IDjpeg_set_defaults_cinfo := 3%positive.
Definition jpeg_set_defaults : graph := {|
  g_start := 1%positive;
  g_end := 18%positive;
  g_edges := (1%positive,(AAssign IDjpeg_set_defaults_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,5%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,7%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::(7%positive,ANone,8%positive)::
             (7%positive,ANone,9%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDjpeg_set_defaults_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_set_defaults_i) s) <
             (eval (ENum (16)) s))%Z)),19%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_set_defaults_i) s) >=
             (eval (ENum (16)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,16%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,18%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDjpeg_set_defaults_i
             (Some (EAdd (EVar IDjpeg_set_defaults_i) (ENum (1))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDjpeg_set_defaults_z
             (Some (EAdd (ENum (1)) (EVar IDjpeg_set_defaults_z)))),
             25%positive)::(25%positive,AWeaken,12%positive)::nil
|}.

Definition jpeg_set_defaults_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 5%positive => (1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 9%positive => (1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_set_defaults_i) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) <= 0 /\ 1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 12%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -16 <= 0)%Z
    | 13%positive => (1 * (s IDjpeg_set_defaults_i) + -16 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 16 <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_set_defaults_i) + 16 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -16 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_set_defaults_i) + -16 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 16 <= 0)%Z
    | 16%positive => (1 * (s IDjpeg_set_defaults_i) + -16 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 16 <= 0)%Z
    | 17%positive => (-1 * (s IDjpeg_set_defaults_i) + 16 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -16 <= 0)%Z
    | 18%positive => (1 * (s IDjpeg_set_defaults_i) + -16 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 16 <= 0)%Z
    | 19%positive => (-1 * (s IDjpeg_set_defaults_i) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -15 <= 0)%Z
    | 20%positive => (1 * (s IDjpeg_set_defaults_i) + -15 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) <= 0)%Z
    | 21%positive => (-1 * (s IDjpeg_set_defaults_i) <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -15 <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 1 <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -16 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_set_defaults_i) + -16 <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 1 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_set_defaults_z) <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 1 <= 0 /\ 1 * (s IDjpeg_set_defaults_i) + -16 <= 0)%Z
    | 25%positive => (1 * (s IDjpeg_set_defaults_i) + -16 <= 0 /\ -1 * (s IDjpeg_set_defaults_i) + 1 <= 0 /\ -1 * (s IDjpeg_set_defaults_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_set_defaults_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 3%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 4%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 5%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 6%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 7%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 8%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 9%positive => ((16 # 1) + (s IDjpeg_set_defaults_z))%Q
    | 10%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 11%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 12%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 13%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 14%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 15%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 16%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 17%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 18%positive => ((s IDjpeg_set_defaults_z))%Q
    | 19%positive => ((s IDjpeg_set_defaults_z)
                      + max0(16 - (s IDjpeg_set_defaults_i)))%Q
    | 20%positive => ((16 # 1) - (s IDjpeg_set_defaults_i)
                      + (s IDjpeg_set_defaults_z))%Q
    | 21%positive => ((16 # 1) - (s IDjpeg_set_defaults_i)
                      + (s IDjpeg_set_defaults_z))%Q
    | 22%positive => ((17 # 1) - (s IDjpeg_set_defaults_i)
                      + (s IDjpeg_set_defaults_z))%Q
    | 23%positive => ((17 # 1) - (s IDjpeg_set_defaults_i)
                      + (s IDjpeg_set_defaults_z))%Q
    | 24%positive => ((17 # 1) - (s IDjpeg_set_defaults_i)
                      + (s IDjpeg_set_defaults_z))%Q
    | 25%positive => ((16 # 1) - (s IDjpeg_set_defaults_i)
                      + (s IDjpeg_set_defaults_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_set_defaults_hints (p : node) (s : state) := 
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
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDjpeg_set_defaults_i)) (15
                                                                    - (s IDjpeg_set_defaults_i)));
                      (*-1 0*) F_max0_ge_0 (15 - (s IDjpeg_set_defaults_i))]
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDjpeg_set_defaults_i)) (15
                                                                    - (s IDjpeg_set_defaults_i)));
                      (*-1 0*) F_max0_ge_0 (15 - (s IDjpeg_set_defaults_i))]
    | 18%positive => []
    | 19%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                  - (s IDjpeg_set_defaults_i))) (F_check_ge (16
                                                                    - (s IDjpeg_set_defaults_i)) (0))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - (s IDjpeg_set_defaults_i)) (0))) (F_max0_ge_0 (16
                                                                    - (s IDjpeg_set_defaults_i)))]
    | _ => []
  end.


Theorem jpeg_set_defaults_ai_correct:
  forall s p' s', steps (g_start jpeg_set_defaults) s (g_edges jpeg_set_defaults) p' s' -> jpeg_set_defaults_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_set_defaults_pot_correct:
  forall s p' s',
    steps (g_start jpeg_set_defaults) s (g_edges jpeg_set_defaults) p' s' ->
    (jpeg_set_defaults_pot (g_start jpeg_set_defaults) s >= jpeg_set_defaults_pot p' s')%Q.
Proof.
  check_lp jpeg_set_defaults_ai_correct jpeg_set_defaults_hints.
Qed.

