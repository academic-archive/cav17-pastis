Require Import pasta.Pasta.

Notation IDjpeg_idct_float_z := 1%positive.
Notation IDjpeg_idct_float__tmp := 2%positive.
Notation IDjpeg_idct_float_ctr := 3%positive.
Notation IDjpeg_idct_float_cinfo := 4%positive.
Notation IDjpeg_idct_float_coef_block := 5%positive.
Notation IDjpeg_idct_float_compptr := 6%positive.
Notation IDjpeg_idct_float_output_buf := 7%positive.
Notation IDjpeg_idct_float_output_col := 8%positive.
Definition jpeg_idct_float : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDjpeg_idct_float_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_idct_float__tmp
             (Some (EVar IDjpeg_idct_float_output_col))),3%positive)::
             (3%positive,(AAssign IDjpeg_idct_float_ctr (Some (ENum (8)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_idct_float_ctr) s) >
             (eval (ENum (0)) s))%Z)),21%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_idct_float_ctr) s) <=
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDjpeg_idct_float_ctr (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_idct_float_ctr) s) <
             (eval (ENum (8)) s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_idct_float_ctr) s) >=
             (eval (ENum (8)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDjpeg_idct_float_ctr
             (Some (EAdd (EVar IDjpeg_idct_float_ctr) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDjpeg_idct_float_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_idct_float_z)))),20%positive)::
             (20%positive,AWeaken,11%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,24%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,25%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDjpeg_idct_float_ctr
             (Some (EAdd (EVar IDjpeg_idct_float_ctr) (ENum (-1))))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDjpeg_idct_float_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_idct_float_z)))),29%positive)::
             (29%positive,AWeaken,6%positive)::nil
|}.

Definition jpeg_idct_float_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 8 <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_idct_float_ctr) + 8 <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_z) <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0)%Z
    | 7%positive => (-1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) <= 0)%Z
    | 8%positive => (1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_idct_float_ctr) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 8 <= 0)%Z
    | 13%positive => (-1 * (s IDjpeg_idct_float_ctr) + 8 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -7 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_idct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -7 <= 0)%Z
    | 17%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0)%Z
    | 18%positive => (1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0)%Z
    | 19%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0)%Z
    | 20%positive => (1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_float_z) + 1 <= 0)%Z
    | 21%positive => (1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_idct_float_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0)%Z
    | 24%positive => (1 * (s IDjpeg_idct_float_ctr) + -8 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDjpeg_idct_float_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -8 <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -7 <= 0)%Z
    | 27%positive => (1 * (s IDjpeg_idct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) <= 0)%Z
    | 28%positive => (-1 * (s IDjpeg_idct_float_z) <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0 /\ 1 * (s IDjpeg_idct_float_ctr) + -7 <= 0)%Z
    | 29%positive => (1 * (s IDjpeg_idct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_idct_float_ctr) <= 0 /\ -1 * (s IDjpeg_idct_float_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_idct_float_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDjpeg_idct_float_z))%Q
    | 3%positive => ((16 # 1) + (s IDjpeg_idct_float_z))%Q
    | 4%positive => ((8 # 1) + (s IDjpeg_idct_float_z)
                     + max0((s IDjpeg_idct_float_ctr)))%Q
    | 5%positive => ((8 # 1) + (s IDjpeg_idct_float_z)
                     + max0((s IDjpeg_idct_float_ctr)))%Q
    | 6%positive => ((8 # 1) + (s IDjpeg_idct_float_z)
                     + max0((s IDjpeg_idct_float_ctr)))%Q
    | 7%positive => ((8 # 1) + (s IDjpeg_idct_float_z)
                     + max0((s IDjpeg_idct_float_ctr)))%Q
    | 8%positive => ((8 # 1) + (s IDjpeg_idct_float_z))%Q
    | 9%positive => ((s IDjpeg_idct_float_z)
                     + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 10%positive => ((s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 11%positive => ((s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 12%positive => ((s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 13%positive => ((s IDjpeg_idct_float_z))%Q
    | 14%positive => ((s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 15%positive => ((1 # 1) + (s IDjpeg_idct_float_z)
                      + max0(7 - (s IDjpeg_idct_float_ctr)))%Q
    | 16%positive => ((1 # 1) + (s IDjpeg_idct_float_z)
                      + max0(7 - (s IDjpeg_idct_float_ctr)))%Q
    | 17%positive => ((1 # 1) + (s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 18%positive => ((1 # 1) + (s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 19%positive => ((1 # 1) + (s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 20%positive => ((s IDjpeg_idct_float_z)
                      + max0(8 - (s IDjpeg_idct_float_ctr)))%Q
    | 21%positive => ((8 # 1) + (s IDjpeg_idct_float_z)
                      + max0((s IDjpeg_idct_float_ctr)))%Q
    | 22%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0(-1 + (s IDjpeg_idct_float_ctr)))%Q
    | 23%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0(-1 + (s IDjpeg_idct_float_ctr)))%Q
    | 24%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0(-1 + (s IDjpeg_idct_float_ctr)))%Q
    | 25%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0(-1 + (s IDjpeg_idct_float_ctr)))%Q
    | 26%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0((s IDjpeg_idct_float_ctr)))%Q
    | 27%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0((s IDjpeg_idct_float_ctr)))%Q
    | 28%positive => ((9 # 1) + (s IDjpeg_idct_float_z)
                      + max0((s IDjpeg_idct_float_ctr)))%Q
    | 29%positive => ((8 # 1) + (s IDjpeg_idct_float_z)
                      + max0((s IDjpeg_idct_float_ctr)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_idct_float_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDjpeg_idct_float_ctr)) (-1
                                                                    + (s IDjpeg_idct_float_ctr)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDjpeg_idct_float_ctr))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                                             - (s IDjpeg_idct_float_ctr)) (7
                                                                    - (s IDjpeg_idct_float_ctr)));
                      (*-1 0*) F_max0_ge_0 (7 - (s IDjpeg_idct_float_ctr))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement (8
                                                     - (s IDjpeg_idct_float_ctr)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_pre_decrement ((s IDjpeg_idct_float_ctr)) (1)]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | _ => []
  end.


Theorem jpeg_idct_float_ai_correct:
  forall s p' s', steps (g_start jpeg_idct_float) s (g_edges jpeg_idct_float) p' s' -> jpeg_idct_float_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_idct_float_pot_correct:
  forall s p' s',
    steps (g_start jpeg_idct_float) s (g_edges jpeg_idct_float) p' s' ->
    (jpeg_idct_float_pot (g_start jpeg_idct_float) s >= jpeg_idct_float_pot p' s')%Q.
Proof.
  check_lp jpeg_idct_float_ai_correct jpeg_idct_float_hints.
Qed.

