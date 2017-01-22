Require Import pasta.Pasta.

Notation IDdebug_dump_bitmap_z := 1%positive.
Notation IDdebug_dump_bitmap__tmp := 2%positive.
Notation IDdebug_dump_bitmap__tmp1 := 3%positive.
Notation IDdebug_dump_bitmap_y := 4%positive.
Notation IDdebug_dump_bitmap_bits := 5%positive.
Notation IDdebug_dump_bitmap_height := 6%positive.
Notation IDdebug_dump_bitmap_msg := 7%positive.
Notation IDdebug_dump_bitmap_raster := 8%positive.
Definition debug_dump_bitmap : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDdebug_dump_bitmap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_bitmap_y) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_bitmap__tmp) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDdebug_dump_bitmap__tmp1
             (Some (EVar IDdebug_dump_bitmap_raster))),6%positive)::
             (6%positive,(AAssign IDdebug_dump_bitmap__tmp
             (Some (EVar IDdebug_dump_bitmap_height))),7%positive)::
             (7%positive,(AAssign IDdebug_dump_bitmap_y (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_bitmap_y) s) <
             (eval (EVar IDdebug_dump_bitmap__tmp) s))%Z)),13%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_bitmap_y) s) >=
             (eval (EVar IDdebug_dump_bitmap__tmp) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_bitmap_y) s) =
             (eval (ENum (0)) s))%Z)),17%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_bitmap_y) s) <>
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,19%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDdebug_dump_bitmap_y
             (Some (EAdd (EVar IDdebug_dump_bitmap_y) (ENum (1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDdebug_dump_bitmap_z
             (Some (EAdd (ENum (1)) (EVar IDdebug_dump_bitmap_z)))),
             24%positive)::(24%positive,AWeaken,10%positive)::nil
|}.

Definition debug_dump_bitmap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdebug_dump_bitmap_z) <= 0 /\ 1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 4%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ 1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDdebug_dump_bitmap__tmp) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ 1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 6%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ 1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDdebug_dump_bitmap_z) <= 0 /\ 1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 8%positive => (1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ 1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 9%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ 1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ 1 * (s IDdebug_dump_bitmap_z) <= 0)%Z
    | 10%positive => (-1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 11%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ 1 * (s IDdebug_dump_bitmap__tmp)+ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 12%positive => (1 * (s IDdebug_dump_bitmap__tmp)+ -1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 13%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 15%positive => (-1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0)%Z
    | 17%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ 1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 18%positive => (1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 19%positive => (-1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) <= 0)%Z
    | 21%positive => (-1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) <= 0)%Z
    | 23%positive => (-1 * (s IDdebug_dump_bitmap_z) <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_y) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDdebug_dump_bitmap_y) + 1 <= 0 /\ -1 * (s IDdebug_dump_bitmap__tmp)+ 1 * (s IDdebug_dump_bitmap_y) <= 0 /\ -1 * (s IDdebug_dump_bitmap_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition debug_dump_bitmap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDdebug_dump_bitmap_height)))%Q
    | 2%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap_height)))%Q
    | 3%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap_height)))%Q
    | 4%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap_height)))%Q
    | 5%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap_height)))%Q
    | 6%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap_height)))%Q
    | 7%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap__tmp)))%Q
    | 8%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap__tmp)
                            - (s IDdebug_dump_bitmap_y)))%Q
    | 9%positive => ((s IDdebug_dump_bitmap_z)
                     + max0((s IDdebug_dump_bitmap__tmp)
                            - (s IDdebug_dump_bitmap_y)))%Q
    | 10%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 11%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 12%positive => ((s IDdebug_dump_bitmap_z))%Q
    | 13%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 14%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 15%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 16%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0(-1 + (s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 17%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 18%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0(-1 + (s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 19%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0(-1 + (s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 20%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0(-1 + (s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 21%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 22%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 23%positive => ((1 # 1) + (s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | 24%positive => ((s IDdebug_dump_bitmap_z)
                      + max0((s IDdebug_dump_bitmap__tmp)
                             - (s IDdebug_dump_bitmap_y)))%Q
    | _ => (0 # 1)%Q
  end.

Definition debug_dump_bitmap_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDdebug_dump_bitmap__tmp)
                                                             - (s IDdebug_dump_bitmap_y)) (-1
                                                                    + (s IDdebug_dump_bitmap__tmp)
                                                                    - (s IDdebug_dump_bitmap_y)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDdebug_dump_bitmap__tmp)
                                            - (s IDdebug_dump_bitmap_y))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*0 1*) F_max0_pre_decrement ((s IDdebug_dump_bitmap__tmp)
                                                    - (s IDdebug_dump_bitmap_y)) (1)]
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_pre_decrement ((s IDdebug_dump_bitmap__tmp)
                                                     - (s IDdebug_dump_bitmap_y)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | _ => []
  end.


Theorem debug_dump_bitmap_ai_correct:
  forall s p' s', steps (g_start debug_dump_bitmap) s (g_edges debug_dump_bitmap) p' s' -> debug_dump_bitmap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem debug_dump_bitmap_pot_correct:
  forall s p' s',
    steps (g_start debug_dump_bitmap) s (g_edges debug_dump_bitmap) p' s' ->
    (debug_dump_bitmap_pot (g_start debug_dump_bitmap) s >= debug_dump_bitmap_pot p' s')%Q.
Proof.
  check_lp debug_dump_bitmap_ai_correct debug_dump_bitmap_hints.
Qed.

