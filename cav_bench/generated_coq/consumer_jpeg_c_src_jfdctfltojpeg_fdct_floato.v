Require Import pasta.Pasta.

Notation IDjpeg_fdct_float_z := 1%positive.
Notation IDjpeg_fdct_float_ctr := 2%positive.
Notation IDjpeg_fdct_float_data := 3%positive.
Definition jpeg_fdct_float : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDjpeg_fdct_float_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjpeg_fdct_float_ctr (Some (ENum (7)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_float_ctr) s) >=
             (eval (ENum (0)) s))%Z)),20%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_float_ctr) s) <
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDjpeg_fdct_float_ctr (Some (ENum (7)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_float_ctr) s) >=
             (eval (ENum (0)) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_fdct_float_ctr) s) <
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDjpeg_fdct_float_ctr
             (Some (EAdd (EVar IDjpeg_fdct_float_ctr) (ENum (-1))))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDjpeg_fdct_float_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_fdct_float_z)))),19%positive)::
             (19%positive,AWeaken,10%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDjpeg_fdct_float_ctr
             (Some (EAdd (EVar IDjpeg_fdct_float_ctr) (ENum (-1))))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDjpeg_fdct_float_z (Some (EAdd (ENum (1))
             (EVar IDjpeg_fdct_float_z)))),26%positive)::
             (26%positive,AWeaken,5%positive)::nil
|}.

Definition jpeg_fdct_float_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + 7 <= 0)%Z
    | 4%positive => (-1 * (s IDjpeg_fdct_float_ctr) + 7 <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ 1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 6%positive => (-1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_fdct_float_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + 7 <= 0)%Z
    | 9%positive => (-1 * (s IDjpeg_fdct_float_ctr) + 7 <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 11%positive => (-1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + 1 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_fdct_float_ctr) + 1 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 13%positive => (1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) <= 0)%Z
    | 14%positive => (-1 * (s IDjpeg_fdct_float_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0)%Z
    | 15%positive => (1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 17%positive => (-1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0)%Z
    | 18%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 19%positive => (-1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) <= 0)%Z
    | 21%positive => (-1 * (s IDjpeg_fdct_float_ctr) <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0)%Z
    | 22%positive => (1 * (s IDjpeg_fdct_float_ctr) + -7 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) <= 0)%Z
    | 23%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) <= 0)%Z
    | 25%positive => (-1 * (s IDjpeg_fdct_float_z) <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0)%Z
    | 26%positive => (-1 * (s IDjpeg_fdct_float_ctr) + -1 <= 0 /\ 1 * (s IDjpeg_fdct_float_ctr) + -6 <= 0 /\ -1 * (s IDjpeg_fdct_float_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_fdct_float_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDjpeg_fdct_float_z))%Q
    | 3%positive => ((8 # 1) + (s IDjpeg_fdct_float_z)
                     + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 4%positive => ((8 # 1) + (s IDjpeg_fdct_float_z)
                     + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 5%positive => ((8 # 1) + (s IDjpeg_fdct_float_z)
                     + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 6%positive => ((8 # 1) + (s IDjpeg_fdct_float_z)
                     + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 7%positive => ((8 # 1) + (s IDjpeg_fdct_float_z))%Q
    | 8%positive => ((s IDjpeg_fdct_float_z)
                     + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 9%positive => ((s IDjpeg_fdct_float_z)
                     + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 10%positive => ((s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 11%positive => ((s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 12%positive => ((s IDjpeg_fdct_float_z))%Q
    | 13%positive => ((s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 14%positive => ((1 # 1) + (s IDjpeg_fdct_float_z)
                      + max0((s IDjpeg_fdct_float_ctr)))%Q
    | 15%positive => ((1 # 1) + (s IDjpeg_fdct_float_z)
                      + max0((s IDjpeg_fdct_float_ctr)))%Q
    | 16%positive => ((1 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 17%positive => ((1 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 18%positive => ((1 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 19%positive => ((s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 20%positive => ((8 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 21%positive => ((9 # 1) + (s IDjpeg_fdct_float_z)
                      + max0((s IDjpeg_fdct_float_ctr)))%Q
    | 22%positive => ((9 # 1) + (s IDjpeg_fdct_float_z)
                      + max0((s IDjpeg_fdct_float_ctr)))%Q
    | 23%positive => ((9 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 24%positive => ((9 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 25%positive => ((9 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | 26%positive => ((8 # 1) + (s IDjpeg_fdct_float_z)
                      + max0(1 + (s IDjpeg_fdct_float_ctr)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_fdct_float_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDjpeg_fdct_float_ctr)) ((s IDjpeg_fdct_float_ctr)));
                     (*-1 0*) F_max0_ge_0 ((s IDjpeg_fdct_float_ctr))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDjpeg_fdct_float_ctr)) ((s IDjpeg_fdct_float_ctr)));
                      (*-1 0*) F_max0_ge_0 ((s IDjpeg_fdct_float_ctr))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDjpeg_fdct_float_ctr)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDjpeg_fdct_float_ctr)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | _ => []
  end.


Theorem jpeg_fdct_float_ai_correct:
  forall s p' s', steps (g_start jpeg_fdct_float) s (g_edges jpeg_fdct_float) p' s' -> jpeg_fdct_float_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_fdct_float_pot_correct:
  forall s p' s',
    steps (g_start jpeg_fdct_float) s (g_edges jpeg_fdct_float) p' s' ->
    (jpeg_fdct_float_pot (g_start jpeg_fdct_float) s >= jpeg_fdct_float_pot p' s')%Q.
Proof.
  check_lp jpeg_fdct_float_ai_correct jpeg_fdct_float_hints.
Qed.

