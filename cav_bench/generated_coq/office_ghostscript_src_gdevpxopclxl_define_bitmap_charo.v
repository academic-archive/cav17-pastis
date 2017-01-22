Require Import pasta.Pasta.

Notation IDpclxl_define_bitmap_char_z := 1%positive.
Notation IDpclxl_define_bitmap_char__tmp := 2%positive.
Notation IDpclxl_define_bitmap_char__tmp1 := 3%positive.
Notation IDpclxl_define_bitmap_char__tmp2 := 4%positive.
Notation IDpclxl_define_bitmap_char__tmp3 := 5%positive.
Notation IDpclxl_define_bitmap_char_i := 6%positive.
Notation IDpclxl_define_bitmap_char_size := 7%positive.
Notation IDpclxl_define_bitmap_char_width_bytes := 8%positive.
Notation IDpclxl_define_bitmap_char_ccode := 9%positive.
Notation IDpclxl_define_bitmap_char_data := 10%positive.
Notation IDpclxl_define_bitmap_char_height := 11%positive.
Notation IDpclxl_define_bitmap_char_raster := 12%positive.
Notation IDpclxl_define_bitmap_char_width_bits := 13%positive.
Notation IDpclxl_define_bitmap_char_xdev := 14%positive.
Definition pclxl_define_bitmap_char : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDpclxl_define_bitmap_char_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_define_bitmap_char_size) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_define_bitmap_char_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_define_bitmap_char__tmp) s) >=
             (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDpclxl_define_bitmap_char__tmp3
             (Some (EVar IDpclxl_define_bitmap_char_ccode))),7%positive)::
             (7%positive,(AAssign IDpclxl_define_bitmap_char__tmp2
             (Some (EVar IDpclxl_define_bitmap_char_raster))),8%positive)::
             (8%positive,(AAssign IDpclxl_define_bitmap_char__tmp1
             (Some (EVar IDpclxl_define_bitmap_char_width_bits))),9%positive)::
             (9%positive,(AAssign IDpclxl_define_bitmap_char__tmp
             (Some (EVar IDpclxl_define_bitmap_char_height))),10%positive)::
             (10%positive,(AAssign IDpclxl_define_bitmap_char_width_bytes
             None),11%positive)::
             (11%positive,(AAssign IDpclxl_define_bitmap_char_size
             (Some (EAdd (ENum (10))
             (EMul (EVar IDpclxl_define_bitmap_char_width_bytes)
             (EVar IDpclxl_define_bitmap_char__tmp))))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,17%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,18%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDpclxl_define_bitmap_char_i
             (Some (ENum (0)))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_define_bitmap_char_i) s) <
             (eval (EVar IDpclxl_define_bitmap_char__tmp) s))%Z)),
             26%positive)::
             (23%positive,(AGuard
             (fun s => ((eval (EVar IDpclxl_define_bitmap_char_i) s) >=
             (eval (EVar IDpclxl_define_bitmap_char__tmp) s))%Z)),
             24%positive)::(24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDpclxl_define_bitmap_char_i
             (Some (EAdd (EVar IDpclxl_define_bitmap_char_i) (ENum (1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDpclxl_define_bitmap_char_z
             (Some (EAdd (ENum (1)) (EVar IDpclxl_define_bitmap_char_z)))),
             32%positive)::(32%positive,AWeaken,23%positive)::nil
|}.

Definition pclxl_define_bitmap_char_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0)%Z
    | 4%positive => (-1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 5%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp) <= 0)%Z
    | 6%positive => (-1 * (s IDpclxl_define_bitmap_char__tmp) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 7%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDpclxl_define_bitmap_char__tmp) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 9%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp) <= 0)%Z
    | 10%positive => (-1 * (s IDpclxl_define_bitmap_char_size) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 11%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_size) <= 0)%Z
    | 12%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 13%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 14%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 15%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 16%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 17%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 18%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 19%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 20%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 21%positive => (1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 22%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 23%positive => (-1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 24%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ 1 * (s IDpclxl_define_bitmap_char__tmp)+ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 25%positive => (1 * (s IDpclxl_define_bitmap_char__tmp)+ -1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 26%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 28%positive => (-1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 30%positive => (-1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) <= 0)%Z
    | 31%positive => (-1 * (s IDpclxl_define_bitmap_char_z) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0 /\ -1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) <= 0)%Z
    | 32%positive => (-1 * (s IDpclxl_define_bitmap_char__tmp)+ 1 * (s IDpclxl_define_bitmap_char_i) <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_i) + 1 <= 0 /\ -1 * (s IDpclxl_define_bitmap_char_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition pclxl_define_bitmap_char_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 2%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 3%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 4%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 5%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 6%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 7%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 8%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 9%positive => ((s IDpclxl_define_bitmap_char_z)
                     + max0((s IDpclxl_define_bitmap_char_height)))%Q
    | 10%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 11%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 12%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 13%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 14%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 15%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 16%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 17%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 18%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 19%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 20%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)))%Q
    | 21%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 22%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 23%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 24%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 25%positive => ((s IDpclxl_define_bitmap_char_z))%Q
    | 26%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 27%positive => ((1 # 1) + (s IDpclxl_define_bitmap_char_z)
                      + max0(-1 + (s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 28%positive => ((1 # 1) + (s IDpclxl_define_bitmap_char_z)
                      + max0(-1 + (s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 29%positive => ((1 # 1) + (s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 30%positive => ((1 # 1) + (s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 31%positive => ((1 # 1) + (s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | 32%positive => ((s IDpclxl_define_bitmap_char_z)
                      + max0((s IDpclxl_define_bitmap_char__tmp)
                             - (s IDpclxl_define_bitmap_char_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition pclxl_define_bitmap_char_hints (p : node) (s : state) := 
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
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpclxl_define_bitmap_char__tmp)
                                                             - (s IDpclxl_define_bitmap_char_i)) (-1
                                                                    + (s IDpclxl_define_bitmap_char__tmp)
                                                                    - (s IDpclxl_define_bitmap_char_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDpclxl_define_bitmap_char__tmp)
                                            - (s IDpclxl_define_bitmap_char_i))]
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_pre_decrement ((s IDpclxl_define_bitmap_char__tmp)
                                                     - (s IDpclxl_define_bitmap_char_i)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | _ => []
  end.


Theorem pclxl_define_bitmap_char_ai_correct:
  forall s p' s', steps (g_start pclxl_define_bitmap_char) s (g_edges pclxl_define_bitmap_char) p' s' -> pclxl_define_bitmap_char_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pclxl_define_bitmap_char_pot_correct:
  forall s p' s',
    steps (g_start pclxl_define_bitmap_char) s (g_edges pclxl_define_bitmap_char) p' s' ->
    (pclxl_define_bitmap_char_pot (g_start pclxl_define_bitmap_char) s >= pclxl_define_bitmap_char_pot p' s')%Q.
Proof.
  check_lp pclxl_define_bitmap_char_ai_correct pclxl_define_bitmap_char_hints.
Qed.

